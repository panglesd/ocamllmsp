(** HTTPS client for the Claude API, built on Lwt and cohttp-lwt-unix.

    Same API as {!Claude_eio}, with results in [Lwt.t].

    {[
      let main () =
        let client = Claude_lwt.create () in
        let req = Claude.Request.make [ Claude.Message.user "Hello!" ] in
        Lwt.map
          (function
            | Ok resp -> print_endline (Claude.Response.text resp)
            | Error e -> prerr_endline (Claude.Api_error.to_string e))
          (Claude_lwt.messages_create client req)

      let () = Lwt_main.run (main ())
    ]}

    TLS is provided by conduit-lwt-unix. It uses OCaml-TLS when [tls-lwt]
    is installed, and OpenSSL (via [lwt_ssl]) otherwise. *)

type auth = Claude.Transport.auth =
  | Api_key of string  (** Sent as [x-api-key]. *)
  | Bearer of string  (** OAuth access token, sent as [Authorization: Bearer]. *)

type t

val default_api_version : string

val create :
  ?ctx:Cohttp_lwt_unix.Client.ctx ->
  ?auth:auth ->
  ?base_url:string ->
  ?api_version:string ->
  ?max_retries:int ->
  unit ->
  t
(** Credentials default to [ANTHROPIC_API_KEY], then [ANTHROPIC_AUTH_TOKEN];
    the base URL to [ANTHROPIC_BASE_URL], then [https://api.anthropic.com].
    Requests failing with a connection error or HTTP 408/409/429/5xx are
    retried [max_retries] times (default 2) with exponential backoff,
    honouring [retry-after]. [ctx] customises the conduit connection context.

    @raise Invalid_argument if no credentials are found. *)

val messages_create :
  t -> Claude.Request.t -> (Claude.Response.t, Claude.Api_error.t) result Lwt.t
(** [POST /v1/messages]. Prefer {!messages_stream} for long outputs or large
    [max_tokens]. *)

val messages_stream :
  ?on_event:(Claude.Stream_event.t -> unit Lwt.t) ->
  t ->
  Claude.Request.t ->
  (Claude.Response.t, Claude.Api_error.t) result Lwt.t
(** Streaming [POST /v1/messages]. [on_event] is called on each event as it
    arrives (and awaited before reading on); the accumulated final message
    is returned. *)

val count_tokens : t -> Claude.Request.t -> (int, Claude.Api_error.t) result Lwt.t
(** [POST /v1/messages/count_tokens]: input tokens of the request. *)

val run_tools :
  ?max_iterations:int ->
  ?stream:bool ->
  ?on_event:(Claude.Stream_event.t -> unit Lwt.t) ->
  t ->
  handler:(name:string -> input:Claude.json -> (string, string) result Lwt.t) ->
  Claude.Request.t ->
  (Claude.Response.t * Claude.Message.t list, Claude.Api_error.t) result Lwt.t
(** Agentic loop: sends the request, runs [handler] on each tool call
    (concurrently when Claude makes several calls at once; [Error msg] or an
    exception is reported to Claude as a failed tool result), sends the
    results back, and repeats until Claude stops calling tools (or
    [max_iterations], default 20, is reached). Also continues on
    [pause_turn]. Returns the final response and the full conversation. *)
