open Claude

let key = Sys.getenv "anthropic_key"
let print_prompt p = Format.eprintf "Sending prompt: %s\n\n%!" p

let request env prompt =
  print_prompt prompt;
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

let list_schema =
  `Assoc
    [
      ("type", `String "object");
      ( "properties",
        `Assoc
          [
            ( "items",
              `Assoc
                [
                  ("type", `String "array");
                  ("items", `Assoc [ ("type", `String "string") ]);
                ] );
          ] );
      ("required", `List [ `String "items" ]);
      ("additionalProperties", `Bool false);
    ]

let request_list env prompt =
  print_prompt prompt;
  let client =
    let auth = Claude.Transport.Api_key key in
    Claude_eio.create ~auth ~net:(Eio.Stdenv.net env)
      ~clock:(Eio.Stdenv.clock env) ()
  in
  let req =
    let model = Claude.Model.haiku_5_5 in
    Request.make ~output_schema:list_schema ~model [ Message.user prompt ]
  in
  match Claude_eio.messages_create client req with
  | Error e -> Error (Api_error.to_string e)
  | Ok resp -> (
      match resp.stop_reason with
      | Some Refusal -> Error "Model refused"
      | _ ->
          Yojson.Safe.from_string (Response.text resp)
          |> Yojson.Safe.Util.member "items"
          |> Yojson.Safe.Util.to_list
          |> List.map Yojson.Safe.Util.to_string
          |> fun s -> Ok s)
