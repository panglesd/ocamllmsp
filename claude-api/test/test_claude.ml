open Claude

let json = Alcotest.testable Yojson.Safe.pp Yojson.Safe.equal
let parse = Yojson.Safe.from_string

let member k = function
  | `Assoc l -> Option.value ~default:`Null (List.assoc_opt k l)
  | _ -> `Null

let test_simple_request () =
  let req = Request.make ~system:"Be brief." [ Message.user "Hi" ] in
  let j = Request.to_json req in
  Alcotest.(check json) "model" (`String "claude-opus-5-5") (member "model" j);
  Alcotest.(check json) "max_tokens" (`Int 16000) (member "max_tokens" j);
  Alcotest.(check json) "system" (`String "Be brief.") (member "system" j);
  Alcotest.(check json)
    "messages"
    (parse {|[{"role":"user","content":"Hi"}]|})
    (member "messages" j);
  Alcotest.(check json) "fallbacks default" (`String "default") (member "fallbacks" j);
  Alcotest.(check (list string))
    "betas" [ "server-side-fallback-2026-07-01" ] (Request.betas req);
  Alcotest.(check json) "no stream" `Null (member "stream" j)

let test_full_request () =
  let tool =
    Tool.make ~name:"get_weather" ~description:"Weather" ~strict:true
      ~input_schema:(parse {|{"type":"object","properties":{}}|})
      ()
  in
  let req =
    Request.make ~model:Model.haiku_5_5 ~max_tokens:100 ~tools:[ tool ]
      ~tool_choice:(Tool.Auto { disable_parallel_tool_use = true })
      ~thinking:(Thinking.adaptive ~display:Summarized ())
      ~effort:High ~output_schema:(`Assoc [ ("type", `String "object") ])
      ~extra:[ ("max_tokens", `Int 5) ]
      [ Message.user "Hi" ]
  in
  let j = Request.to_json ~stream:true req in
  Alcotest.(check json) "no fallbacks on haiku" `Null (member "fallbacks" j);
  Alcotest.(check (list string)) "no betas" [] (Request.betas req);
  Alcotest.(check json) "extra overrides" (`Int 5) (member "max_tokens" j);
  Alcotest.(check json) "stream" (`Bool true) (member "stream" j);
  Alcotest.(check json)
    "thinking"
    (parse {|{"type":"adaptive","display":"summarized"}|})
    (member "thinking" j);
  Alcotest.(check json)
    "output_config"
    (parse {|{"effort":"high","format":{"type":"json_schema","schema":{"type":"object"}}}|})
    (member "output_config" j);
  Alcotest.(check json)
    "tools"
    (parse
       {|[{"name":"get_weather","description":"Weather","input_schema":{"type":"object","properties":{}},"strict":true}]|})
    (member "tools" j);
  Alcotest.(check json)
    "tool_choice"
    (parse {|{"type":"auto","disable_parallel_tool_use":true}|})
    (member "tool_choice" j);
  let c = Request.to_json ~for_count_tokens:true (Request.make [ Message.user "x" ]) in
  Alcotest.(check json) "count_tokens: no max_tokens" `Null (member "max_tokens" c);
  Alcotest.(check json) "count_tokens: no fallbacks" `Null (member "fallbacks" c)

let response_json =
  {|{"id":"msg_1","type":"message","role":"assistant","model":"claude-opus-5-5",
     "content":[{"type":"thinking","thinking":"","signature":"sig"},
                {"type":"text","text":"Let me check."},
                {"type":"tool_use","id":"toolu_1","name":"get_weather","input":{"city":"Paris"}},
                {"type":"fallback","from":{"model":"a"},"to":{"model":"b"}}],
     "stop_reason":"tool_use","stop_sequence":null,
     "usage":{"input_tokens":10,"output_tokens":20,"cache_read_input_tokens":3}}|}

let test_response () =
  let r = Response.of_json (parse response_json) in
  Alcotest.(check string) "text" "Let me check." (Response.text r);
  Alcotest.(check bool) "stop" true (r.stop_reason = Some Stop_reason.Tool_use);
  Alcotest.(check int) "usage" 3 r.usage.cache_read_input_tokens;
  (match Response.tool_uses r with
  | [ ("toolu_1", "get_weather", input) ] ->
      Alcotest.(check json) "input" (parse {|{"city":"Paris"}|}) input
  | _ -> Alcotest.fail "tool_uses");
  (match List.nth r.content 3 with
  | Block.Other { type_ = "fallback"; _ } -> ()
  | _ -> Alcotest.fail "unknown block kept as Other");
  (* Replay is verbatim. *)
  let m = Message.to_json (Response.to_message r) in
  Alcotest.(check json) "replay" (member "content" (parse response_json)) (member "content" m)

let sse_stream =
  {|event: message_start
data: {"type":"message_start","message":{"id":"msg_2","type":"message","role":"assistant","model":"claude-opus-5-5","content":[],"stop_reason":null,"usage":{"input_tokens":5,"output_tokens":1}}}

event: content_block_start
data: {"type":"content_block_start","index":0,"content_block":{"type":"text","text":""}}

event: ping
data: {"type": "ping"}

event: content_block_delta
data: {"type":"content_block_delta","index":0,"delta":{"type":"text_delta","text":"Hel"}}

event: content_block_delta
data: {"type":"content_block_delta","index":0,"delta":{"type":"text_delta","text":"lo"}}

event: content_block_stop
data: {"type":"content_block_stop","index":0}

event: content_block_start
data: {"type":"content_block_start","index":1,"content_block":{"type":"tool_use","id":"toolu_9","name":"f","input":{}}}

event: content_block_delta
data: {"type":"content_block_delta","index":1,"delta":{"type":"input_json_delta","partial_json":"{\"a\": "}}

event: content_block_delta
data: {"type":"content_block_delta","index":1,"delta":{"type":"input_json_delta","partial_json":"1}"}}

event: content_block_stop
data: {"type":"content_block_stop","index":1}

event: message_delta
data: {"type":"message_delta","delta":{"stop_reason":"tool_use","stop_sequence":null},"usage":{"output_tokens":12}}

event: message_stop
data: {"type":"message_stop"}

|}

let run_stream s =
  let sse = Sse.create () and acc = Accumulator.create () in
  let events = ref [] in
  String.split_on_char '\n' s
  |> List.iter (fun line ->
         match Sse.feed_line sse line with
         | None -> ()
         | Some (event, data) ->
             let ev = Stream_event.of_sse ~event ~data in
             events := ev :: !events;
             Accumulator.add acc ev);
  (List.rev !events, Accumulator.finish acc)

let test_stream () =
  let events, result = run_stream sse_stream in
  Alcotest.(check int) "event count" 12 (List.length events);
  match result with
  | Error e -> Alcotest.fail (Api_error.to_string e)
  | Ok r ->
      Alcotest.(check string) "text" "Hello" (Response.text r);
      Alcotest.(check bool) "stop" true (r.stop_reason = Some Tool_use);
      Alcotest.(check int) "input tokens kept" 5 r.usage.input_tokens;
      Alcotest.(check int) "output tokens updated" 12 r.usage.output_tokens;
      (match Response.tool_uses r with
      | [ ("toolu_9", "f", input) ] ->
          Alcotest.(check json) "input" (parse {|{"a":1}|}) input
      | _ -> Alcotest.fail "tool_uses")

let test_stream_chunks () =
  (* Same stream, split into awkward chunks (mid-line, mid-CRLF). *)
  let s = String.concat "\r\n" (String.split_on_char '\n' sse_stream) in
  let sse = Sse.create () and acc = Accumulator.create () in
  let n = String.length s in
  let rec go i size =
    if i < n then (
      let len = min size (n - i) in
      List.iter
        (fun (event, data) -> Accumulator.add acc (Stream_event.of_sse ~event ~data))
        (Sse.feed_chunk sse (String.sub s i len));
      go (i + len) ((size * 7 mod 13) + 1))
  in
  go 0 3;
  match Accumulator.finish acc with
  | Ok r -> Alcotest.(check string) "text" "Hello" (Response.text r)
  | Error e -> Alcotest.fail (Api_error.to_string e)

let test_stream_error () =
  let s =
    "event: error\r\ndata: {\"type\":\"error\",\"error\":{\"type\":\"overloaded_error\",\"message\":\"Overloaded\"}}\r\n\r\n"
  in
  match run_stream s with
  | _, Error (Api_error.Stream { type_ = "overloaded_error"; _ }) -> ()
  | _ -> Alcotest.fail "expected stream error"

let test_api_error () =
  match
    Api_error.of_body ~status:429 ~request_id:(Some "req_1")
      {|{"type":"error","error":{"type":"rate_limit_error","message":"slow down"}}|}
  with
  | Http { type_ = "rate_limit_error"; message = "slow down"; _ } as e ->
      Alcotest.(check bool) "retryable" true (Api_error.is_retryable e)
  | _ -> Alcotest.fail "of_body"

let () =
  Alcotest.run "claude"
    [
      ( "request",
        [
          Alcotest.test_case "simple" `Quick test_simple_request;
          Alcotest.test_case "full" `Quick test_full_request;
        ] );
      ( "response",
        [
          Alcotest.test_case "decode" `Quick test_response;
          Alcotest.test_case "api error" `Quick test_api_error;
        ] );
      ( "stream",
        [
          Alcotest.test_case "accumulate" `Quick test_stream;
          Alcotest.test_case "chunked" `Quick test_stream_chunks;
          Alcotest.test_case "error event" `Quick test_stream_error;
        ] );
    ]
