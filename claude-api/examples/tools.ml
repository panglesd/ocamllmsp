(* Tool use: Claude calls an OCaml function. *)
open Claude

let weather_tool =
  Tool.make ~name:"get_weather" ~description:"Get the current weather for a city."
    ~strict:true
    ~input_schema:
      (Yojson.Safe.from_string
         {|{"type":"object",
            "properties":{"city":{"type":"string","description":"City name"}},
            "required":["city"],
            "additionalProperties":false}|})
    ()

let handler ~name ~input =
  match (name, input) with
  | "get_weather", `Assoc fields -> (
      match List.assoc_opt "city" fields with
      | Some (`String city) -> Ok (Printf.sprintf "18°C and sunny in %s" city)
      | _ -> Error "missing city")
  | _ -> Error ("unknown tool " ^ name)

let () =
  Eio_main.run @@ fun env ->
  let client =
    Claude_eio.create ~net:(Eio.Stdenv.net env) ~clock:(Eio.Stdenv.clock env) ()
  in
  let req =
    Request.make ~tools:[ weather_tool ]
      [ Message.user "What's the weather like in Paris and in Lyon?" ]
  in
  match Claude_eio.run_tools client ~handler req with
  | Ok (resp, conversation) ->
      Printf.eprintf "[%d messages in conversation]\n" (List.length conversation);
      print_endline (Response.text resp)
  | Error e ->
      prerr_endline (Api_error.to_string e);
      exit 1
