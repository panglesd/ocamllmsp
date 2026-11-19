(* Multi-turn streaming chat in the terminal. *)
open Claude

let () =
  Eio_main.run @@ fun env ->
  let client =
    Claude_eio.create ~net:(Eio.Stdenv.net env) ~clock:(Eio.Stdenv.clock env) ()
  in
  let on_event = function
    | Stream_event.Content_block_delta { delta = Text_delta s; _ } ->
        print_string s;
        flush stdout
    | _ -> ()
  in
  let rec loop history =
    print_string "\n> ";
    flush stdout;
    match In_channel.input_line stdin with
    | None | Some ("exit" | "quit") -> ()
    | Some line -> (
        let history = history @ [ Message.user line ] in
        let req =
          Request.make ~max_tokens:64000
            ~system:"You are a helpful assistant. Be concise."
            ~cache_control:Ephemeral history
        in
        match Claude_eio.messages_stream ~on_event client req with
        | Ok resp ->
            print_newline ();
            loop (history @ [ Response.to_message resp ])
        | Error e ->
            prerr_endline ("\nError: " ^ Api_error.to_string e);
            loop history)
  in
  loop []
