(** Typed OCaml interface to the Claude Messages API.

    This module is I/O-agnostic: it builds request bodies, decodes responses
    and parses Server-Sent Events. Use {!Claude_eio} (library
    [claude-api.eio]) to actually talk to the API. *)

type json = Yojson.Safe.t

exception Decode_error of string

(** {1 Models} *)

module Model : sig
  type t = string

  val opus_5_5 : t
  val sonnet_5_5 : t
  val haiku_5_5 : t
  val fable_5_2 : t

  val default : t
  (** [claude-opus-5-5]. *)

  val supports_server_fallbacks : t -> bool
end

(** {1 Building requests} *)

module Cache_control : sig
  type t = Ephemeral  (** 5 minutes *) | Ephemeral_1h

  val to_json : t -> json
end

(** Content blocks that you send. *)
module Content : sig
  type source =
    | Base64 of { media_type : string; data : string }
    | Url of string
    | File of string  (** A Files API [file_id]. *)
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
    | Raw of json  (** Sent verbatim; escape hatch for any other block. *)

  val text : ?cache_control:Cache_control.t -> string -> t

  val image : ?cache_control:Cache_control.t -> media_type:string -> string -> t
  (** [image ~media_type b64] with base64-encoded data (no newlines). *)

  val image_url : ?cache_control:Cache_control.t -> string -> t

  val pdf :
    ?cache_control:Cache_control.t -> ?title:string -> ?citations:bool -> string -> t
  (** Base64-encoded PDF. *)

  val text_document :
    ?cache_control:Cache_control.t -> ?title:string -> ?citations:bool -> string -> t

  val file_document :
    ?cache_control:Cache_control.t -> ?title:string -> ?citations:bool -> string -> t

  val tool_result :
    ?cache_control:Cache_control.t ->
    ?is_error:bool ->
    tool_use_id:string ->
    string ->
    t

  val to_json : t -> json
end

module Message : sig
  type role =
    | User
    | Assistant
    | System
        (** Mid-conversation operator instruction (supported on current
            models; must follow a user message). *)

  type t = { role : role; content : Content.t list }

  val make : role -> Content.t list -> t
  val user : string -> t
  val assistant : string -> t
  val system : string -> t
  val to_json : t -> json
end

module Tool : sig
  type t =
    | Custom of {
        name : string;
        description : string option;
        input_schema : json;  (** A JSON Schema object. *)
        strict : bool;
        eager_input_streaming : bool;
        cache_control : Cache_control.t option;
      }
    | Builtin of json  (** Anthropic-defined / server tools, sent verbatim. *)

  val make :
    ?description:string ->
    ?strict:bool ->
    ?eager_input_streaming:bool ->
    ?cache_control:Cache_control.t ->
    name:string ->
    input_schema:json ->
    unit ->
    t

  val web_search : ?max_uses:int -> unit -> t
  val web_fetch : ?max_uses:int -> unit -> t
  val code_execution : unit -> t

  type choice =
    | Auto of { disable_parallel_tool_use : bool }
    | Any  (** Rejected (400) by Opus 5.5, Sonnet 5.5, Fable 5.1/5.2. *)
    | Named of string  (** Rejected (400) by Opus 5.5, Sonnet 5.5, Fable 5.1/5.2. *)
    | No_tools

  val to_json : t -> json
end

module Thinking : sig
  type display = Summarized | Omitted

  type t =
    | Adaptive of { display : display option }
    | Disabled  (** Rejected (400) by Opus 5.5, Sonnet 5.5 and Fable. *)
    | Between_tools  (** Sonnet 5.5 only. *)

  val adaptive : ?display:display -> unit -> t
end

module Effort : sig
  type t = Low | Medium | High | Xhigh | Max

  val to_string : t -> string
end

(** Server-side refusal fallbacks: when a safety classifier declines a
    request, the API re-runs it on a fallback model within the same call. *)
module Fallbacks : sig
  type t =
    | Default  (** Let the API route by refusal category. *)
    | Models of Model.t list
    | Disabled

  val beta : string
end

module Request : sig
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

  val make :
    ?model:Model.t ->
    ?max_tokens:int ->
    ?system:string ->
    ?system_blocks:Content.t list ->
    ?tools:Tool.t list ->
    ?tool_choice:Tool.choice ->
    ?thinking:Thinking.t ->
    ?effort:Effort.t ->
    ?output_schema:json ->
    ?stop_sequences:string list ->
    ?user_id:string ->
    ?cache_control:Cache_control.t ->
    ?fallbacks:Fallbacks.t ->
    ?betas:string list ->
    ?extra:(string * json) list ->
    Message.t list ->
    t
  (** Defaults: [model = Model.default] ([claude-opus-5-5]),
      [max_tokens = 16000] (use a larger value with streaming).

      - [output_schema]: structured output; the response text is JSON matching
        this JSON Schema.
      - [cache_control]: top-level automatic prompt caching.
      - [fallbacks]: defaults to {!Fallbacks.Default} on models that support
        server-side fallbacks, {!Fallbacks.Disabled} otherwise. The required
        beta header is added automatically.
      - [extra]: additional top-level body fields (override generated ones),
        for API features not modelled here. *)

  val with_messages : t -> Message.t list -> t

  val betas : t -> string list
  (** All [anthropic-beta] values the request needs. *)

  val to_json : ?stream:bool -> ?for_count_tokens:bool -> t -> json
end

(** {1 Responses} *)

(** Content blocks returned by the API. *)
module Block : sig
  type t =
    | Text of { text : string; citations : json list }
    | Thinking of { thinking : string; signature : string }
    | Redacted_thinking of string
    | Tool_use of { id : string; name : string; input : json }
    | Other of { type_ : string; json : json }
        (** Server tool blocks, fallback blocks, compaction, ... *)

  val of_json : json -> t
end

module Stop_reason : sig
  type t =
    | End_turn
    | Max_tokens
    | Stop_sequence
    | Tool_use
    | Pause_turn
    | Refusal
    | Other of string

  val of_string : string -> t
  val to_string : t -> string
end

module Usage : sig
  type t = {
    input_tokens : int;
    output_tokens : int;
    cache_creation_input_tokens : int;
    cache_read_input_tokens : int;
  }
end

module Response : sig
  type stop_details = { category : string option; explanation : string option }

  type t = {
    id : string;
    model : Model.t;  (** The model that actually produced the message. *)
    content : Block.t list;
    stop_reason : Stop_reason.t option;
    stop_sequence : string option;
    stop_details : stop_details option;  (** Set when [stop_reason = Refusal]. *)
    usage : Usage.t;
    raw : json;  (** The full JSON response. *)
  }

  val of_json : json -> t

  val text : t -> string
  (** Concatenation of all text blocks. *)

  val tool_uses : t -> (string * string * json) list
  (** [(id, name, input)] of each tool call. *)

  val to_message : t -> Message.t
  (** The response as an assistant message to append to the conversation.
      Blocks are replayed verbatim, so thinking blocks and server-tool blocks
      remain valid. *)
end

module Api_error : sig
  type t =
    | Http of {
        status : int;
        type_ : string;
        message : string;
        request_id : string option;
      }
    | Stream of { type_ : string; message : string }
        (** An [error] event received mid-stream. *)
    | Connection of string
    | Decode of string

  val to_string : t -> string
  val pp : Format.formatter -> t -> unit
  val is_retryable : t -> bool
  val of_body : status:int -> request_id:string option -> string -> t
end

val decode : ('a -> 'b) -> 'a -> ('b, Api_error.t) result
(** Runs a decoder, turning decoding exceptions into [Api_error.Decode]. *)

(** {1 Streaming} *)

(** Incremental Server-Sent Events parser. *)
module Sse : sig
  type t

  val create : unit -> t

  val feed_line : t -> string -> (string * string) option
  (** Feed one line (without its terminator). Returns [Some (event, data)]
      when a blank line completes an event. *)

  val feed_chunk : t -> string -> (string * string) list
  (** Feed an arbitrary chunk of the byte stream (lines may be split across
      chunks). Returns the events completed by this chunk. *)
end

module Stream_event : sig
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

  val of_sse : event:string -> data:string -> t
end

(** Rebuilds the final {!Response.t} from a sequence of stream events. *)
module Accumulator : sig
  type t

  val create : unit -> t
  val add : t -> Stream_event.t -> unit
  val finish : t -> (Response.t, Api_error.t) result
end

(** {1 Backend helpers}

    Building blocks shared by {!Claude_eio} and {!Claude_lwt}; useful if you
    write a client for another I/O library. *)
module Transport : sig
  type auth =
    | Api_key of string  (** Sent as [x-api-key]. *)
    | Bearer of string  (** OAuth access token, sent as [Authorization: Bearer]. *)

  val default_api_version : string
  val default_base_url : string

  val auth_from_env : unit -> auth option
  (** [ANTHROPIC_API_KEY], then [ANTHROPIC_AUTH_TOKEN]. *)

  val resolve_auth : auth option -> auth
  (** @raise Invalid_argument if no credentials are given or found. *)

  val resolve_base_url : string option -> string
  (** Falls back to [ANTHROPIC_BASE_URL], then {!default_base_url}. *)

  val headers :
    auth:auth ->
    api_version:string ->
    betas:string list ->
    body_length:int ->
    (string * string) list

  val backoff_delay : attempt:int -> retry_after:string option -> float
  val parse_response : string -> (Response.t, Api_error.t) result
  val parse_count_tokens : string -> (int, Api_error.t) result

  val handle_sse : Accumulator.t -> string * string -> Stream_event.t option
  val is_terminal : Stream_event.t -> bool

  type tool_step =
    | Continue of Message.t list
    | Run_tools of Message.t list * (string * string * json) list
    | Done of Message.t list

  val tool_step :
    max_iterations:int -> iteration:int -> Message.t list -> Response.t -> tool_step

  val tool_result_content : string * (string, string) result -> Content.t
end
