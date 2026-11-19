(* Tool use with Lwt: handlers return promises and run concurrently. *)
open Claude
open Lwt.Syntax

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
      | Some (`String city) ->
          let* () = Lwt_unix.sleep 0.1 (* pretend to call a weather service *) in
          Lwt.return (Ok (Printf.sprintf "18°C and sunny in %s" city))
      | _ -> Lwt.return (Error "missing city"))
  | _ -> Lwt.return (Error ("unknown tool " ^ name))

let main () =
  let client = Claude_lwt.create () in
  let req =
    Request.make ~tools:[ weather_tool ]
      [ Message.user "What's the weather like in Paris and in Lyon?" ]
  in
  let+ r = Claude_lwt.run_tools client ~handler req in
  match r with
  | Ok (resp, _) -> print_endline (Response.text resp)
  | Error e ->
      prerr_endline (Api_error.to_string e);
      exit 1

let () = Lwt_main.run (main ())
