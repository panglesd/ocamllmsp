(* Single request with Lwt. Run: dune exec examples/lwt/hello_lwt.exe -- "prompt" *)
open Claude
open Lwt.Syntax

let main prompt =
  let client = Claude_lwt.create () in
  let+ r = Claude_lwt.messages_create client (Request.make [ Message.user prompt ]) in
  match r with
  | Error e ->
      prerr_endline (Api_error.to_string e);
      exit 1
  | Ok resp -> (
      match resp.stop_reason with
      | Some Refusal -> prerr_endline "Refused."
      | _ ->
          print_endline (Response.text resp);
          Printf.eprintf "[%s, %d in / %d out tokens]\n" resp.model
            resp.usage.input_tokens resp.usage.output_tokens)

let () =
  let prompt =
    if Array.length Sys.argv > 1 then Sys.argv.(1)
    else "In one sentence, what is OCaml?"
  in
  Lwt_main.run (main prompt)
