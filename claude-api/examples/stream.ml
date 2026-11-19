(* Streaming: prints text as it arrives. *)
open Claude

let () =
  let prompt =
    if Array.length Sys.argv > 1 then Sys.argv.(1)
    else "Write a short poem about functional programming."
  in
  Eio_main.run @@ fun env ->
  let client =
    Claude_eio.create ~net:(Eio.Stdenv.net env) ~clock:(Eio.Stdenv.clock env) ()
  in
  let req = Request.make ~max_tokens:64000 [ Message.user prompt ] in
  let on_event = function
    | Stream_event.Content_block_delta { delta = Text_delta s; _ } ->
        print_string s;
        flush stdout
    | _ -> ()
  in
  match Claude_eio.messages_stream ~on_event client req with
  | Ok resp ->
      print_newline ();
      Printf.eprintf "[stop: %s]\n"
        (Option.fold ~none:"-" ~some:Stop_reason.to_string resp.stop_reason)
  | Error e ->
      prerr_endline (Api_error.to_string e);
      exit 1
