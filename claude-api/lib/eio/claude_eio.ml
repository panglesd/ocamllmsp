open Claude

type auth = Transport.auth = Api_key of string | Bearer of string

type t = {
  http : Cohttp_eio.Client.t;
  clock : float Eio.Time.clock_ty Eio.Resource.t;
  auth : auth;
  base_url : string;
  api_version : string;
  max_retries : int;
}

let default_api_version = Transport.default_api_version
let rng_initialised = ref false

let https () =
  if not !rng_initialised then (
    Mirage_crypto_rng_unix.use_default ();
    rng_initialised := true);
  let authenticator =
    match Ca_certs.authenticator () with
    | Ok a -> a
    | Error (`Msg m) -> failwith ("Claude_eio: cannot load CA certificates: " ^ m)
  in
  let config =
    match Tls.Config.client ~authenticator () with
    | Ok c -> c
    | Error (`Msg m) -> failwith ("Claude_eio: invalid TLS configuration: " ^ m)
  in
  fun uri raw ->
    let host =
      Option.bind (Uri.host uri) (fun h ->
          Result.to_option (Result.bind (Domain_name.of_string h) Domain_name.host))
    in
    Tls_eio.client_of_flow ?host config raw

let create ?auth ?base_url ?(api_version = default_api_version)
    ?(max_retries = 2) ~net ~clock () =
  {
    http = Cohttp_eio.Client.make ~https:(Some (https ())) net;
    clock = (clock :> float Eio.Time.clock_ty Eio.Resource.t);
    auth = Transport.resolve_auth auth;
    base_url = Transport.resolve_base_url base_url;
    api_version;
    max_retries;
  }

let read_all body = Eio.Buf_read.(parse_exn take_all) body ~max_size:max_int

(* Performs a POST and hands a 2xx response body to [on_success], retrying
   transient failures (connection errors, 408/409/429/5xx) with backoff. *)
let post t ~path ~betas ~json
    ~(on_success : Cohttp_eio.Body.t -> ('a, Api_error.t) result) =
  let uri = Uri.of_string (t.base_url ^ path) in
  let body_s = Yojson.Safe.to_string json in
  let headers =
    Http.Header.of_list
      (Transport.headers ~auth:t.auth ~api_version:t.api_version ~betas
         ~body_length:(String.length body_s))
  in
  let attempt_once () =
    Eio.Switch.run @@ fun sw ->
    match
      Cohttp_eio.Client.post t.http ~sw ~chunked:false ~headers
        ~body:(Cohttp_eio.Body.of_string body_s) uri
    with
    | exception (Eio.Cancel.Cancelled _ as e) -> raise e
    | exception e -> (Error (Api_error.Connection (Printexc.to_string e)), None)
    | resp, body -> (
        let status = Http.Status.to_int (Http.Response.status resp) in
        let hdrs = Http.Response.headers resp in
        if status >= 200 && status < 300 then
          match on_success body with
          | r -> (r, None)
          | exception (Eio.Cancel.Cancelled _ as e) -> raise e
          | exception e -> (Error (Api_error.Connection (Printexc.to_string e)), None)
        else
          let request_id = Http.Header.get hdrs "request-id" in
          let body = try read_all body with _ -> "" in
          ( Error (Api_error.of_body ~status ~request_id body),
            Http.Header.get hdrs "retry-after" ))
  in
  let rec loop attempt =
    match attempt_once () with
    | Ok v, _ -> Ok v
    | Error e, retry_after when Api_error.is_retryable e && attempt < t.max_retries ->
        Eio.Time.sleep t.clock (Transport.backoff_delay ~attempt ~retry_after);
        loop (attempt + 1)
    | (Error _ as e), _ -> e
  in
  loop 0

let messages_create t (req : Request.t) =
  post t ~path:"/v1/messages" ~betas:(Request.betas req)
    ~json:(Request.to_json req) ~on_success:(fun body ->
      Transport.parse_response (read_all body))

let messages_stream ?(on_event = fun _ -> ()) t (req : Request.t) =
  post t ~path:"/v1/messages" ~betas:(Request.betas req)
    ~json:(Request.to_json ~stream:true req) ~on_success:(fun body ->
      let r = Eio.Buf_read.of_flow body ~max_size:max_int in
      let sse = Sse.create () in
      let acc = Accumulator.create () in
      let rec loop () =
        match Eio.Buf_read.line r with
        | exception End_of_file -> ()
        | line -> (
            match Option.bind (Sse.feed_line sse line) (Transport.handle_sse acc) with
            | None -> loop ()
            | Some ev ->
                on_event ev;
                if not (Transport.is_terminal ev) then loop ())
      in
      loop ();
      Accumulator.finish acc)

let count_tokens t (req : Request.t) =
  post t ~path:"/v1/messages/count_tokens" ~betas:req.betas
    ~json:(Request.to_json ~for_count_tokens:true req) ~on_success:(fun body ->
      Transport.parse_count_tokens (read_all body))

let run_tools ?(max_iterations = 20) ?(stream = false) ?on_event t ~handler
    (req : Request.t) =
  let send req =
    if stream then messages_stream ?on_event t req else messages_create t req
  in
  let run_one (id, name, input) =
    let result =
      match handler ~name ~input with
      | r -> r
      | exception (Eio.Cancel.Cancelled _ as e) -> raise e
      | exception e -> Error (Printexc.to_string e)
    in
    Transport.tool_result_content (id, result)
  in
  let rec loop messages iteration =
    match send (Request.with_messages req messages) with
    | Error _ as e -> e
    | Ok resp -> (
        match Transport.tool_step ~max_iterations ~iteration messages resp with
        | Done messages -> Ok (resp, messages)
        | Continue messages -> loop messages (iteration + 1)
        | Run_tools (messages, calls) ->
            let results = List.map run_one calls in
            loop (messages @ [ Message.make User results ]) (iteration + 1))
  in
  loop req.messages 0
