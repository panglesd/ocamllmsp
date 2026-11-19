open Claude

let key = Sys.getenv "anthropic_key"

let request env prompt =
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
  | Error e -> Error (Api_error.to_string e)
  | Ok resp -> (
      match resp.stop_reason with
      | Some Refusal -> Error "Model refused"
      | _ -> Ok (Response.text resp))
