(* Streaming with Lwt: prints text as it arrives. *)
open Claude
open Lwt.Syntax

let on_event = function
  | Stream_event.Content_block_delta { delta = Text_delta s; _ } ->
      let* () = Lwt_io.print s in
      Lwt_io.flush Lwt_io.stdout
  | _ -> Lwt.return_unit

let main prompt =
  let client = Claude_lwt.create () in
  let req = Request.make ~max_tokens:64000 [ Message.user prompt ] in
  let* r = Claude_lwt.messages_stream ~on_event client req in
  match r with
  | Ok _ -> Lwt_io.printl ""
  | Error e ->
      prerr_endline (Api_error.to_string e);
      exit 1

let () =
  let prompt =
    if Array.length Sys.argv > 1 then Sys.argv.(1)
    else "Write a short poem about functional programming."
  in
  Lwt_main.run (main prompt)
