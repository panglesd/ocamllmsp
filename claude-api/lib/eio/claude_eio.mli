(** HTTPS client for the Claude API, built on Eio and cohttp-eio.

    {[
      Eio_main.run @@ fun env ->
      let client =
        Claude_eio.create ~net:(Eio.Stdenv.net env)
          ~clock:(Eio.Stdenv.clock env) ()
      in
      let req = Claude.Request.make [ Claude.Message.user "Hello!" ] in
      match Claude_eio.messages_create client req with
      | Ok resp -> print_endline (Claude.Response.text resp)
      | Error e -> prerr_endline (Claude.Api_error.to_string e)
    ]} *)

type auth = Claude.Transport.auth =
  | Api_key of string  (** Sent as [x-api-key]. *)
  | Bearer of string  (** OAuth access token, sent as [Authorization: Bearer]. *)

type t

val default_api_version : string
(** The [anthropic-version] header value, ["2026-10-21"]. *)

val create :
  ?auth:auth ->
  ?base_url:string ->
  ?api_version:string ->
  ?max_retries:int ->
  net:_ Eio.Net.t ->
  clock:_ Eio.Time.clock ->
  unit ->
  t
(** Credentials default to [ANTHROPIC_API_KEY], then [ANTHROPIC_AUTH_TOKEN];
    the base URL to [ANTHROPIC_BASE_URL], then [https://api.anthropic.com].
    Requests failing with a connection error or HTTP 408/409/429/5xx are
    retried [max_retries] times (default 2) with exponential backoff,
    honouring [retry-after].

    @raise Invalid_argument if no credentials are found. *)

val messages_create :
  t -> Claude.Request.t -> (Claude.Response.t, Claude.Api_error.t) result
(** [POST /v1/messages]. Prefer {!messages_stream} for long outputs or large
    [max_tokens]. *)

val messages_stream :
  ?on_event:(Claude.Stream_event.t -> unit) ->
  t ->
  Claude.Request.t ->
  (Claude.Response.t, Claude.Api_error.t) result
(** Streaming [POST /v1/messages]. [on_event] is called on each event as it
    arrives; the accumulated final message is returned. *)

val count_tokens : t -> Claude.Request.t -> (int, Claude.Api_error.t) result
(** [POST /v1/messages/count_tokens]: input tokens of the request. *)

val run_tools :
  ?max_iterations:int ->
  ?stream:bool ->
  ?on_event:(Claude.Stream_event.t -> unit) ->
  t ->
  handler:(name:string -> input:Claude.json -> (string, string) result) ->
  Claude.Request.t ->
  (Claude.Response.t * Claude.Message.t list, Claude.Api_error.t) result
(** Agentic loop: sends the request, runs [handler] on each tool call
    ([Error msg] or an exception is reported to Claude as a failed tool
    result), sends the results back, and repeats until Claude stops calling
    tools (or [max_iterations], default 20, is reached). Also continues on
    [pause_turn]. Returns the final response and the full conversation,
    which can be extended for a follow-up turn. *)
