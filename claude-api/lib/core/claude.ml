type json = Yojson.Safe.t

(* ------------------------------------------------------------------------ *)
(* JSON helpers                                                             *)

exception Decode_error of string

module J = struct
  let fail fmt = Printf.ksprintf (fun s -> raise (Decode_error s)) fmt

  let member k = function
    | `Assoc l -> ( try List.assoc k l with Not_found -> `Null)
    | _ -> `Null

  let string_opt k j = match member k j with `String s -> Some s | _ -> None

  let string k j =
    match member k j with
    | `String s -> s
    | _ -> fail "expected a string field %S" k

  let int_opt k j =
    match member k j with
    | `Int i -> Some i
    | `Intlit s -> int_of_string_opt s
    | `Float f -> Some (int_of_float f)
    | _ -> None

  let int k j =
    match int_opt k j with Some i -> i | None -> fail "expected an int field %S" k

  let list k j = match member k j with `List l -> l | _ -> []
  let opt k f = function None -> [] | Some v -> [ (k, f v) ]
  let if_ cond k v = if cond then [ (k, v) ] else []

  (* Replace or add a field in an object. *)
  let set fields k v = (k, v) :: List.remove_assoc k fields
end

(* ------------------------------------------------------------------------ *)

module Model = struct
  type t = string

  let opus_5_5 = "claude-opus-5-5"
  let sonnet_5_5 = "claude-sonnet-5-5"
  let haiku_5_5 = "claude-haiku-5-5"
  let fable_5_2 = "claude-fable-5-2"
  let default = opus_5_5

  let supports_server_fallbacks m =
    List.mem m
      [
        "claude-opus-5-5";
        "claude-opus-5";
        "claude-sonnet-5-5";
        "claude-fable-5-1";
        "claude-fable-5-2";
      ]
end

module Cache_control = struct
  type t = Ephemeral | Ephemeral_1h

  let to_json = function
    | Ephemeral -> `Assoc [ ("type", `String "ephemeral") ]
    | Ephemeral_1h ->
        `Assoc [ ("type", `String "ephemeral"); ("ttl", `String "1h") ]
end

module Content = struct
  type source =
    | Base64 of { media_type : string; data : string }
    | Url of string
    | File of string
    | Plain_text of string

  type t =
    | Text of { text : string; cache_control : Cache_control.t option }
    | Image of { source : source; cache_control : Cache_control.t option }
    | Document of {
        source : source;
        title : string option;
        citations : bool;
        cache_control : Cache_control.t option;
      }
    | Tool_use of { id : string; name : string; input : json }
    | Tool_result of {
        tool_use_id : string;
        content : t list;
        is_error : bool;
        cache_control : Cache_control.t option;
      }
    | Raw of json

  let text ?cache_control text = Text { text; cache_control }

  let image ?cache_control ~media_type data =
    Image { source = Base64 { media_type; data }; cache_control }

  let image_url ?cache_control url = Image { source = Url url; cache_control }

  let pdf ?cache_control ?title ?(citations = false) data =
    Document
      {
        source = Base64 { media_type = "application/pdf"; data };
        title;
        citations;
        cache_control;
      }

  let text_document ?cache_control ?title ?(citations = false) data =
    Document { source = Plain_text data; title; citations; cache_control }

  let file_document ?cache_control ?title ?(citations = false) file_id =
    Document { source = File file_id; title; citations; cache_control }

  let tool_result ?cache_control ?(is_error = false) ~tool_use_id content =
    Tool_result
      { tool_use_id; content = [ text content ]; is_error; cache_control }

  let source_to_json = function
    | Base64 { media_type; data } ->
        `Assoc
          [
            ("type", `String "base64");
            ("media_type", `String media_type);
            ("data", `String data);
          ]
    | Url url -> `Assoc [ ("type", `String "url"); ("url", `String url) ]
    | File id -> `Assoc [ ("type", `String "file"); ("file_id", `String id) ]
    | Plain_text data ->
        `Assoc
          [
            ("type", `String "text");
            ("media_type", `String "text/plain");
            ("data", `String data);
          ]

  let cc = J.opt "cache_control" Cache_control.to_json

  let rec to_json = function
    | Text { text; cache_control } ->
        `Assoc
          ([ ("type", `String "text"); ("text", `String text) ] @ cc cache_control)
    | Image { source; cache_control } ->
        `Assoc
          ([ ("type", `String "image"); ("source", source_to_json source) ]
          @ cc cache_control)
    | Document { source; title; citations; cache_control } ->
        `Assoc
          ([ ("type", `String "document"); ("source", source_to_json source) ]
          @ J.opt "title" (fun s -> `String s) title
          @ J.if_ citations "citations" (`Assoc [ ("enabled", `Bool true) ])
          @ cc cache_control)
    | Tool_use { id; name; input } ->
        `Assoc
          [
            ("type", `String "tool_use");
            ("id", `String id);
            ("name", `String name);
            ("input", input);
          ]
    | Tool_result { tool_use_id; content; is_error; cache_control } ->
        `Assoc
          ([
             ("type", `String "tool_result");
             ("tool_use_id", `String tool_use_id);
             ("content", `List (List.map to_json content));
           ]
          @ J.if_ is_error "is_error" (`Bool true)
          @ cc cache_control)
    | Raw j -> j
end

module Message = struct
  type role = User | Assistant | System
  type t = { role : role; content : Content.t list }

  let make role content = { role; content }
  let user s = { role = User; content = [ Content.text s ] }
  let assistant s = { role = Assistant; content = [ Content.text s ] }
  let system s = { role = System; content = [ Content.text s ] }

  let role_to_string = function
    | User -> "user"
    | Assistant -> "assistant"
    | System -> "system"

  let to_json { role; content } =
    let content =
      match content with
      | [ Content.Text { text; cache_control = None } ] -> `String text
      | l -> `List (List.map Content.to_json l)
    in
    `Assoc [ ("role", `String (role_to_string role)); ("content", content) ]
end

module Tool = struct
  type t =
    | Custom of {
        name : string;
        description : string option;
        input_schema : json;
        strict : bool;
        eager_input_streaming : bool;
        cache_control : Cache_control.t option;
      }
    | Builtin of json

  let make ?description ?(strict = false) ?(eager_input_streaming = false)
      ?cache_control ~name ~input_schema () =
    Custom
      {
        name;
        description;
        input_schema;
        strict;
        eager_input_streaming;
        cache_control;
      }

  let web_search ?max_uses () =
    Builtin
      (`Assoc
         ([
            ("type", `String "web_search_20260209");
            ("name", `String "web_search");
          ]
         @ J.opt "max_uses" (fun i -> `Int i) max_uses))

  let web_fetch ?max_uses () =
    Builtin
      (`Assoc
         ([
            ("type", `String "web_fetch_20260209");
            ("name", `String "web_fetch");
          ]
         @ J.opt "max_uses" (fun i -> `Int i) max_uses))

  let code_execution () =
    Builtin
      (`Assoc
         [
           ("type", `String "code_execution_20260521");
           ("name", `String "code_execution");
         ])

  let to_json = function
    | Custom
        {
          name;
          description;
          input_schema;
          strict;
          eager_input_streaming;
          cache_control;
        } ->
        `Assoc
          ([ ("name", `String name) ]
          @ J.opt "description" (fun s -> `String s) description
          @ [ ("input_schema", input_schema) ]
          @ J.if_ strict "strict" (`Bool true)
          @ J.if_ eager_input_streaming "eager_input_streaming" (`Bool true)
          @ Content.cc cache_control)
    | Builtin j -> j

  type choice =
    | Auto of { disable_parallel_tool_use : bool }
    | Any
    | Named of string
    | No_tools

  let choice_to_json = function
    | Auto { disable_parallel_tool_use } ->
        `Assoc
          ([ ("type", `String "auto") ]
          @ J.if_ disable_parallel_tool_use "disable_parallel_tool_use"
              (`Bool true))
    | Any -> `Assoc [ ("type", `String "any") ]
    | Named n -> `Assoc [ ("type", `String "tool"); ("name", `String n) ]
    | No_tools -> `Assoc [ ("type", `String "none") ]
end

module Thinking = struct
  type display = Summarized | Omitted
  type t = Adaptive of { display : display option } | Disabled | Between_tools

  let adaptive ?display () = Adaptive { display }

  let to_json = function
    | Adaptive { display } ->
        `Assoc
          ([ ("type", `String "adaptive") ]
          @ J.opt "display"
              (function
                | Summarized -> `String "summarized"
                | Omitted -> `String "omitted")
              display)
    | Disabled -> `Assoc [ ("type", `String "disabled") ]
    | Between_tools -> `Assoc [ ("type", `String "between_tools") ]
end

module Effort = struct
  type t = Low | Medium | High | Xhigh | Max

  let to_string = function
    | Low -> "low"
    | Medium -> "medium"
    | High -> "high"
    | Xhigh -> "xhigh"
    | Max -> "max"
end

module Fallbacks = struct
  type t = Default | Models of Model.t list | Disabled

  let beta = "server-side-fallback-2026-07-01"

  let to_json = function
    | Default -> Some (`String "default")
    | Models l ->
        Some
          (`List (List.map (fun m -> `Assoc [ ("model", `String m) ]) l))
    | Disabled -> None
end

module Request = struct
  type t = {
    model : Model.t;
    max_tokens : int;
    messages : Message.t list;
    system : Content.t list;
    tools : Tool.t list;
    tool_choice : Tool.choice option;
    thinking : Thinking.t option;
    effort : Effort.t option;
    output_schema : json option;
    stop_sequences : string list;
    user_id : string option;
    cache_control : Cache_control.t option;
    fallbacks : Fallbacks.t;
    betas : string list;
    extra : (string * json) list;
  }

  let make ?(model = Model.default) ?(max_tokens = 16000) ?system
      ?(system_blocks = []) ?(tools = []) ?tool_choice ?thinking ?effort
      ?output_schema ?(stop_sequences = []) ?user_id ?cache_control ?fallbacks
      ?(betas = []) ?(extra = []) messages =
    let system =
      match (system, system_blocks) with
      | _, (_ :: _ as blocks) -> blocks
      | Some s, [] -> [ Content.text s ]
      | None, [] -> []
    in
    let fallbacks =
      match fallbacks with
      | Some f -> f
      | None ->
          if Model.supports_server_fallbacks model then Fallbacks.Default
          else Fallbacks.Disabled
    in
    {
      model;
      max_tokens;
      messages;
      system;
      tools;
      tool_choice;
      thinking;
      effort;
      output_schema;
      stop_sequences;
      user_id;
      cache_control;
      fallbacks;
      betas;
      extra;
    }

  let with_messages t messages = { t with messages }

  let betas t =
    let l =
      match t.fallbacks with
      | Disabled -> t.betas
      | _ -> t.betas @ [ Fallbacks.beta ]
    in
    List.sort_uniq String.compare l

  let to_json ?(stream = false) ?(for_count_tokens = false) t =
    let output_config =
      J.opt "effort" (fun e -> `String (Effort.to_string e)) t.effort
      @ J.opt "format"
          (fun schema ->
            `Assoc [ ("type", `String "json_schema"); ("schema", schema) ])
          t.output_schema
    in
    let fields =
      [ ("model", `String t.model) ]
      @ J.if_ (not for_count_tokens) "max_tokens" (`Int t.max_tokens)
      @ (match t.system with
        | [] -> []
        | [ Content.Text { text; cache_control = None } ] ->
            [ ("system", `String text) ]
        | l -> [ ("system", `List (List.map Content.to_json l)) ])
      @ [ ("messages", `List (List.map Message.to_json t.messages)) ]
      @ (match t.tools with
        | [] -> []
        | l -> [ ("tools", `List (List.map Tool.to_json l)) ])
      @ J.opt "tool_choice" Tool.choice_to_json t.tool_choice
      @ J.opt "thinking" Thinking.to_json t.thinking
      @ (match output_config with
        | [] -> []
        | l -> [ ("output_config", `Assoc l) ])
      @ (match t.stop_sequences with
        | [] -> []
        | l -> [ ("stop_sequences", `List (List.map (fun s -> `String s) l)) ])
      @ J.opt "metadata"
          (fun u -> `Assoc [ ("user_id", `String u) ])
          (if for_count_tokens then None else t.user_id)
      @ J.opt "cache_control" Cache_control.to_json t.cache_control
      @ (if for_count_tokens then []
         else
           match Fallbacks.to_json t.fallbacks with
           | None -> []
           | Some j -> [ ("fallbacks", j) ])
      @ J.if_ (stream && not for_count_tokens) "stream" (`Bool true)
    in
    let fields =
      List.fold_left (fun acc (k, v) -> J.set acc k v) fields t.extra
    in
    `Assoc fields
end

(* ------------------------------------------------------------------------ *)
(* Responses                                                                *)

module Block = struct
  type t =
    | Text of { text : string; citations : json list }
    | Thinking of { thinking : string; signature : string }
    | Redacted_thinking of string
    | Tool_use of { id : string; name : string; input : json }
    | Other of { type_ : string; json : json }

  let of_json j =
    match J.string "type" j with
    | "text" -> Text { text = J.string "text" j; citations = J.list "citations" j }
    | "thinking" ->
        Thinking
          {
            thinking = Option.value ~default:"" (J.string_opt "thinking" j);
            signature = Option.value ~default:"" (J.string_opt "signature" j);
          }
    | "redacted_thinking" -> Redacted_thinking (J.string "data" j)
    | "tool_use" ->
        Tool_use
          { id = J.string "id" j; name = J.string "name" j; input = J.member "input" j }
    | type_ -> Other { type_; json = j }
end

module Stop_reason = struct
  type t =
    | End_turn
    | Max_tokens
    | Stop_sequence
    | Tool_use
    | Pause_turn
    | Refusal
    | Other of string

  let of_string = function
    | "end_turn" -> End_turn
    | "max_tokens" -> Max_tokens
    | "stop_sequence" -> Stop_sequence
    | "tool_use" -> Tool_use
    | "pause_turn" -> Pause_turn
    | "refusal" -> Refusal
    | s -> Other s

  let to_string = function
    | End_turn -> "end_turn"
    | Max_tokens -> "max_tokens"
    | Stop_sequence -> "stop_sequence"
    | Tool_use -> "tool_use"
    | Pause_turn -> "pause_turn"
    | Refusal -> "refusal"
    | Other s -> s
end

module Usage = struct
  type t = {
    input_tokens : int;
    output_tokens : int;
    cache_creation_input_tokens : int;
    cache_read_input_tokens : int;
  }

  let of_json j =
    let get k = Option.value ~default:0 (J.int_opt k j) in
    {
      input_tokens = get "input_tokens";
      output_tokens = get "output_tokens";
      cache_creation_input_tokens = get "cache_creation_input_tokens";
      cache_read_input_tokens = get "cache_read_input_tokens";
    }
end

module Response = struct
  type stop_details = { category : string option; explanation : string option }

  type t = {
    id : string;
    model : Model.t;
    content : Block.t list;
    stop_reason : Stop_reason.t option;
    stop_sequence : string option;
    stop_details : stop_details option;
    usage : Usage.t;
    raw : json;
  }

  let of_json j =
    match J.member "type" j with
    | `String "message" | `Null ->
        {
          id = J.string "id" j;
          model = J.string "model" j;
          content = List.map Block.of_json (J.list "content" j);
          stop_reason = Option.map Stop_reason.of_string (J.string_opt "stop_reason" j);
          stop_sequence = J.string_opt "stop_sequence" j;
          stop_details =
            (match J.member "stop_details" j with
            | `Assoc _ as d ->
                Some
                  {
                    category = J.string_opt "category" d;
                    explanation = J.string_opt "explanation" d;
                  }
            | _ -> None);
          usage = Usage.of_json (J.member "usage" j);
          raw = j;
        }
    | _ -> J.fail "not a message object"

  let text t =
    t.content
    |> List.filter_map (function Block.Text { text; _ } -> Some text | _ -> None)
    |> String.concat ""

  let tool_uses t =
    List.filter_map
      (function Block.Tool_use { id; name; input } -> Some (id, name, input) | _ -> None)
      t.content

  (* Replays the content blocks exactly as the API returned them, which keeps
     thinking blocks (and their signatures), server tool blocks, fallback
     blocks, etc. valid when sent back. *)
  let to_message t =
    Message.make Message.Assistant
      (List.map (fun b -> Content.Raw b) (J.list "content" t.raw))
end

module Api_error = struct
  type t =
    | Http of {
        status : int;
        type_ : string;
        message : string;
        request_id : string option;
      }
    | Stream of { type_ : string; message : string }
    | Connection of string
    | Decode of string

  let to_string = function
    | Http { status; type_; message; request_id } ->
        Printf.sprintf "HTTP %d %s: %s%s" status type_ message
          (match request_id with None -> "" | Some r -> " (request-id " ^ r ^ ")")
    | Stream { type_; message } ->
        Printf.sprintf "stream error %s: %s" type_ message
    | Connection m -> "connection error: " ^ m
    | Decode m -> "decode error: " ^ m

  let pp ppf e = Format.pp_print_string ppf (to_string e)

  let is_retryable = function
    | Http { status; _ } -> status = 408 || status = 409 || status = 429 || status >= 500
    | Stream { type_; _ } -> type_ = "overloaded_error" || type_ = "api_error"
    | Connection _ -> true
    | Decode _ -> false

  let of_body ~status ~request_id body =
    let type_, message =
      match Yojson.Safe.from_string body with
      | j ->
          let e = J.member "error" j in
          ( Option.value ~default:"unknown_error" (J.string_opt "type" e),
            Option.value ~default:body (J.string_opt "message" e) )
      | exception _ -> ("unknown_error", body)
    in
    Http { status; type_; message; request_id }
end

let decode f j =
  match f j with
  | v -> Ok v
  | exception Decode_error m -> Error (Api_error.Decode m)
  | exception Yojson.Json_error m -> Error (Api_error.Decode m)

(* ------------------------------------------------------------------------ *)
(* Streaming                                                                *)

module Sse = struct
  type t = {
    mutable event : string option;
    data : Buffer.t;
    mutable has_data : bool;
    pending : Buffer.t; (* incomplete line, for [feed_chunk] *)
  }

  let create () =
    {
      event = None;
      data = Buffer.create 256;
      has_data = false;
      pending = Buffer.create 256;
    }

  let feed_line t line =
    let line =
      let n = String.length line in
      if n > 0 && line.[n - 1] = '\r' then String.sub line 0 (n - 1) else line
    in
    if line = "" then (
      let ev =
        if t.has_data || t.event <> None then
          Some (Option.value ~default:"message" t.event, Buffer.contents t.data)
        else None
      in
      t.event <- None;
      Buffer.clear t.data;
      t.has_data <- false;
      ev)
    else if line.[0] = ':' then None
    else
      let field, value =
        match String.index_opt line ':' with
        | None -> (line, "")
        | Some i ->
            let v = String.sub line (i + 1) (String.length line - i - 1) in
            let v =
              if String.length v > 0 && v.[0] = ' ' then
                String.sub v 1 (String.length v - 1)
              else v
            in
            (String.sub line 0 i, v)
      in
      (match field with
      | "event" -> t.event <- Some value
      | "data" ->
          if t.has_data then Buffer.add_char t.data '\n';
          Buffer.add_string t.data value;
          t.has_data <- true
      | _ -> ());
      None

  let feed_chunk t chunk =
    let events = ref [] in
    let n = String.length chunk in
    let start = ref 0 in
    for i = 0 to n - 1 do
      if chunk.[i] = '\n' then (
        Buffer.add_substring t.pending chunk !start (i - !start);
        let line = Buffer.contents t.pending in
        Buffer.clear t.pending;
        (match feed_line t line with Some e -> events := e :: !events | None -> ());
        start := i + 1)
    done;
    Buffer.add_substring t.pending chunk !start (n - !start);
    List.rev !events
end

module Stream_event = struct
  type delta =
    | Text_delta of string
    | Thinking_delta of string
    | Signature_delta of string
    | Input_json_delta of string
    | Citations_delta of json
    | Other_delta of json

  type t =
    | Message_start of json
    | Content_block_start of { index : int; block : json }
    | Content_block_delta of { index : int; delta : delta }
    | Content_block_stop of int
    | Message_delta of { delta : json; usage : json }
    | Message_stop
    | Ping
    | Error of { type_ : string; message : string }
    | Unknown of { event : string; data : json }

  let delta_of_json d =
    match J.string_opt "type" d with
    | Some "text_delta" -> Text_delta (J.string "text" d)
    | Some "thinking_delta" -> Thinking_delta (J.string "thinking" d)
    | Some "signature_delta" -> Signature_delta (J.string "signature" d)
    | Some "input_json_delta" -> Input_json_delta (J.string "partial_json" d)
    | Some "citations_delta" -> Citations_delta (J.member "citation" d)
    | _ -> Other_delta d

  let of_sse ~event ~data =
    let j = Yojson.Safe.from_string data in
    let kind = match J.string_opt "type" j with Some t -> t | None -> event in
    match kind with
    | "message_start" -> Message_start (J.member "message" j)
    | "content_block_start" ->
        Content_block_start { index = J.int "index" j; block = J.member "content_block" j }
    | "content_block_delta" ->
        Content_block_delta
          { index = J.int "index" j; delta = delta_of_json (J.member "delta" j) }
    | "content_block_stop" -> Content_block_stop (J.int "index" j)
    | "message_delta" ->
        Message_delta { delta = J.member "delta" j; usage = J.member "usage" j }
    | "message_stop" -> Message_stop
    | "ping" -> Ping
    | "error" ->
        let e = J.member "error" j in
        Error
          {
            type_ = Option.value ~default:"unknown_error" (J.string_opt "type" e);
            message = Option.value ~default:data (J.string_opt "message" e);
          }
    | event -> Unknown { event; data = j }
end

module Accumulator = struct
  type block = {
    start : (string * json) list;
    text : Buffer.t;
    thinking : Buffer.t;
    mutable signature : string option;
    input_json : Buffer.t;
    mutable citations : json list; (* reversed *)
  }

  type t = {
    mutable message : (string * json) list;
    blocks : (int, block) Hashtbl.t;
    mutable error : (string * string) option;
  }

  let create () = { message = []; blocks = Hashtbl.create 8; error = None }
  let assoc = function `Assoc l -> l | _ -> []

  let add t (ev : Stream_event.t) =
    match ev with
    | Message_start m -> t.message <- assoc m
    | Content_block_start { index; block } ->
        Hashtbl.replace t.blocks index
          {
            start = assoc block;
            text = Buffer.create 64;
            thinking = Buffer.create 0;
            signature = None;
            input_json = Buffer.create 0;
            citations = [];
          }
    | Content_block_delta { index; delta } -> (
        match Hashtbl.find_opt t.blocks index with
        | None -> ()
        | Some b -> (
            match delta with
            | Text_delta s -> Buffer.add_string b.text s
            | Thinking_delta s -> Buffer.add_string b.thinking s
            | Signature_delta s -> b.signature <- Some s
            | Input_json_delta s -> Buffer.add_string b.input_json s
            | Citations_delta c -> b.citations <- c :: b.citations
            | Other_delta _ -> ()))
    | Content_block_stop _ | Message_stop | Ping | Unknown _ -> ()
    | Message_delta { delta; usage } ->
        List.iter (fun (k, v) -> t.message <- J.set t.message k v) (assoc delta);
        let u = List.fold_left (fun acc (k, v) -> J.set acc k v)
            (assoc (J.member "usage" (`Assoc t.message))) (assoc usage) in
        t.message <- J.set t.message "usage" (`Assoc u)
    | Error { type_; message } -> t.error <- Some (type_, message)

  let finish_block b =
    let f = b.start in
    let append k buf f =
      if Buffer.length buf = 0 then f
      else
        let prev = match List.assoc_opt k f with Some (`String s) -> s | _ -> "" in
        J.set f k (`String (prev ^ Buffer.contents buf))
    in
    let f = append "text" b.text f in
    let f = append "thinking" b.thinking f in
    let f = match b.signature with None -> f | Some s -> J.set f "signature" (`String s) in
    let f =
      if Buffer.length b.input_json = 0 then f
      else J.set f "input" (Yojson.Safe.from_string (Buffer.contents b.input_json))
    in
    let f =
      match b.citations with
      | [] -> f
      | cs ->
          let prev = match List.assoc_opt "citations" f with Some (`List l) -> l | _ -> [] in
          J.set f "citations" (`List (prev @ List.rev cs))
    in
    `Assoc f

  let finish t =
    match t.error with
    | Some (type_, message) -> Error (Api_error.Stream { type_; message })
    | None ->
        decode
          (fun () ->
            let indices =
              Hashtbl.fold (fun i _ acc -> i :: acc) t.blocks [] |> List.sort compare
            in
            let content =
              List.map (fun i -> finish_block (Hashtbl.find t.blocks i)) indices
            in
            Response.of_json (`Assoc (J.set t.message "content" (`List content))))
          ()
end

(* ------------------------------------------------------------------------ *)
(* Helpers shared by the I/O backends                                       *)

module Transport = struct
  type auth = Api_key of string | Bearer of string

  let default_api_version = "2026-10-21"
  let default_base_url = "https://api.anthropic.com"
  let user_agent = "claude-api-ocaml/0.1.0"

  let getenv v = match Sys.getenv_opt v with Some "" | None -> None | s -> s

  let auth_from_env () =
    match (getenv "ANTHROPIC_API_KEY", getenv "ANTHROPIC_AUTH_TOKEN") with
    | Some k, _ -> Some (Api_key k)
    | None, Some t -> Some (Bearer t)
    | None, None -> None

  let resolve_auth = function
    | Some a -> a
    | None -> (
        match auth_from_env () with
        | Some a -> a
        | None ->
            invalid_arg
              "Claude: no credentials (pass ~auth or set ANTHROPIC_API_KEY)")

  let resolve_base_url base_url =
    let u =
      match base_url with
      | Some u -> u
      | None -> Option.value ~default:default_base_url (getenv "ANTHROPIC_BASE_URL")
    in
    if String.ends_with ~suffix:"/" u then String.sub u 0 (String.length u - 1)
    else u

  let headers ~auth ~api_version ~betas ~body_length =
    let betas =
      match auth with Bearer _ -> "oauth-2025-04-20" :: betas | Api_key _ -> betas
    in
    [
      ("content-type", "application/json");
      ("accept", "application/json");
      ("anthropic-version", api_version);
      ("content-length", string_of_int body_length);
      ("user-agent", user_agent);
    ]
    @ (match auth with
      | Api_key k -> [ ("x-api-key", k) ]
      | Bearer tok -> [ ("authorization", "Bearer " ^ tok) ])
    @ match betas with [] -> [] | l -> [ ("anthropic-beta", String.concat "," l) ]

  let backoff_delay ~attempt ~retry_after =
    match Option.bind retry_after float_of_string_opt with
    | Some s when s >= 0. && s <= 60. -> s
    | _ ->
        let base = Float.min 8. (0.5 *. (2. ** float_of_int attempt)) in
        base *. (0.75 +. Random.float 0.25)

  let parse_response body =
    decode (fun s -> Response.of_json (Yojson.Safe.from_string s)) body

  let parse_count_tokens body =
    decode
      (fun s ->
        match J.int_opt "input_tokens" (Yojson.Safe.from_string s) with
        | Some n -> n
        | None -> J.fail "missing input_tokens")
      body

  (* Decodes one SSE event, feeds it to the accumulator, and returns it.
     Malformed events are skipped. *)
  let handle_sse acc (event, data) =
    match Stream_event.of_sse ~event ~data with
    | ev ->
        Accumulator.add acc ev;
        Some ev
    | exception (Decode_error _ | Yojson.Json_error _) -> None

  let is_terminal : Stream_event.t -> bool = function
    | Message_stop | Error _ -> true
    | _ -> false

  type tool_step =
    | Continue of Message.t list
    | Run_tools of Message.t list * (string * string * json) list
    | Done of Message.t list

  let tool_step ~max_iterations ~iteration messages (resp : Response.t) =
    let messages = messages @ [ Response.to_message resp ] in
    match resp.stop_reason with
    | Some Tool_use when iteration < max_iterations ->
        Run_tools (messages, Response.tool_uses resp)
    | Some Pause_turn when iteration < max_iterations -> Continue messages
    | _ -> Done messages

  let tool_result_content (id, result) =
    match result with
    | Ok out -> Content.tool_result ~tool_use_id:id out
    | Error err -> Content.tool_result ~is_error:true ~tool_use_id:id err
end
