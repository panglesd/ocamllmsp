(* Single request. Run: dune exec examples/hello.exe -- "your prompt" *)
open Claude

let key = Sys.getenv "anthropic_key"

let () =
  let prompt =
    if Array.length Sys.argv > 1 then Sys.argv.(1)
    else "In one sentence, what is OCaml?"
  in
  Eio_main.run @@ fun env ->
  let client =
    let auth = Claude.Transport.Api_key key in
    Claude_eio.create ~auth ~net:(Eio.Stdenv.net env)
      ~clock:(Eio.Stdenv.clock env) ()
  in
  let req =
    let model = Claude.Model.haiku_5_5 in
    Request.make ~model [ Message.user prompt ]
  in
  match Claude_eio.messages_create client req with
  | Error e ->
      prerr_endline (Api_error.to_string e);
      exit 1
  | Ok resp -> (
      match resp.stop_reason with
      | Some Refusal ->
          Printf.eprintf "Refused (%s)\n"
            (Option.value ~default:"?"
               (Option.bind resp.stop_details (fun d -> d.category)))
      | _ ->
          print_endline (Response.text resp);
          Printf.eprintf "[%s, %d in / %d out tokens]\n" resp.model
            resp.usage.input_tokens resp.usage.output_tokens)
