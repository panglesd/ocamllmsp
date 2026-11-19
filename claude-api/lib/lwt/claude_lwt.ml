open Claude
open Lwt.Syntax

type auth = Transport.auth = Api_key of string | Bearer of string

type t = {
  ctx : Cohttp_lwt_unix.Client.ctx option;
  auth : auth;
  base_url : string;
  api_version : string;
  max_retries : int;
}

let default_api_version = Transport.default_api_version

let create ?ctx ?auth ?base_url ?(api_version = default_api_version)
    ?(max_retries = 2) () =
  {
    ctx;
    auth = Transport.resolve_auth auth;
    base_url = Transport.resolve_base_url base_url;
    api_version;
    max_retries;
  }

let connection_error e = Error (Api_error.Connection (Printexc.to_string e))

(* Performs a POST and hands a 2xx response body to [on_success], retrying
   transient failures (connection errors, 408/409/429/5xx) with backoff. *)
let post t ~path ~betas ~json
    ~(on_success : Cohttp_lwt.Body.t -> ('a, Api_error.t) result Lwt.t) =
  let uri = Uri.of_string (t.base_url ^ path) in
  let body_s = Yojson.Safe.to_string json in
  let headers =
    Http.Header.of_list
      (Transport.headers ~auth:t.auth ~api_version:t.api_version ~betas
         ~body_length:(String.length body_s))
  in
  let attempt_once () =
    Lwt.catch
      (fun () ->
        let* resp, body =
          Cohttp_lwt_unix.Client.post ?ctx:t.ctx ~chunked:false ~headers
            ~body:(Cohttp_lwt.Body.of_string body_s) uri
        in
        let status = Http.Status.to_int (Http.Response.status resp) in
        let hdrs = Http.Response.headers resp in
        if status >= 200 && status < 300 then
          let+ r = on_success body in
          (r, None)
        else
          let request_id = Http.Header.get hdrs "request-id" in
          let+ body = Cohttp_lwt.Body.to_string body in
          ( Error (Api_error.of_body ~status ~request_id body),
            Http.Header.get hdrs "retry-after" ))
      (function
        | Lwt.Canceled as e -> Lwt.reraise e
        | e -> Lwt.return (connection_error e, None))
  in
  let rec loop attempt =
    let* r = attempt_once () in
    match r with
    | Ok v, _ -> Lwt.return (Ok v)
    | Error e, retry_after when Api_error.is_retryable e && attempt < t.max_retries ->
        let* () = Lwt_unix.sleep (Transport.backoff_delay ~attempt ~retry_after) in
        loop (attempt + 1)
    | (Error _ as e), _ -> Lwt.return e
  in
  loop 0

let messages_create t (req : Request.t) =
  post t ~path:"/v1/messages" ~betas:(Request.betas req)
    ~json:(Request.to_json req) ~on_success:(fun body ->
      let+ s = Cohttp_lwt.Body.to_string body in
      Transport.parse_response s)

let messages_stream ?(on_event = fun _ -> Lwt.return_unit) t (req : Request.t) =
  post t ~path:"/v1/messages" ~betas:(Request.betas req)
    ~json:(Request.to_json ~stream:true req) ~on_success:(fun body ->
      let chunks = Cohttp_lwt.Body.to_stream body in
      let sse = Sse.create () in
      let acc = Accumulator.create () in
      let rec handle = function
        | [] -> Lwt.return `More
        | raw :: rest -> (
            match Transport.handle_sse acc raw with
            | None -> handle rest
            | Some ev ->
                let* () = on_event ev in
                if Transport.is_terminal ev then Lwt.return `Stop else handle rest)
      in
      let rec loop () =
        let* chunk = Lwt_stream.get chunks in
        match chunk with
        | None -> Lwt.return_unit
        | Some chunk -> (
            let* r = handle (Sse.feed_chunk sse chunk) in
            match r with `More -> loop () | `Stop -> Lwt.return_unit)
      in
      let* () = loop () in
      let+ () = Cohttp_lwt.Body.drain_body (`Stream chunks) in
      Accumulator.finish acc)

let count_tokens t (req : Request.t) =
  post t ~path:"/v1/messages/count_tokens" ~betas:req.betas
    ~json:(Request.to_json ~for_count_tokens:true req) ~on_success:(fun body ->
      let+ s = Cohttp_lwt.Body.to_string body in
      Transport.parse_count_tokens s)

let run_tools ?(max_iterations = 20) ?(stream = false) ?on_event t ~handler
    (req : Request.t) =
  let send req =
    if stream then messages_stream ?on_event t req else messages_create t req
  in
  let run_one (id, name, input) =
    let+ result =
      Lwt.catch
        (fun () -> handler ~name ~input)
        (function
          | Lwt.Canceled as e -> Lwt.reraise e
          | e -> Lwt.return (Error (Printexc.to_string e)))
    in
    Transport.tool_result_content (id, result)
  in
  let rec loop messages iteration =
    let* r = send (Request.with_messages req messages) in
    match r with
    | Error _ as e -> Lwt.return e
    | Ok resp -> (
        match Transport.tool_step ~max_iterations ~iteration messages resp with
        | Done messages -> Lwt.return (Ok (resp, messages))
        | Continue messages -> loop messages (iteration + 1)
        | Run_tools (messages, calls) ->
            let* results = Lwt_list.map_p run_one calls in
            loop (messages @ [ Message.make User results ]) (iteration + 1))
  in
  loop req.messages 0
