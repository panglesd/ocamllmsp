# claude-api — OCaml bindings to the Claude API

Typed OCaml interface to the [Claude Messages API](https://platform.claude.com/docs).

- **`claude-api`** (module `Claude`): I/O-agnostic core: request builders,
  response decoding, SSE parsing and stream accumulation. Depends only on `yojson`.
- **`claude-api.eio`** (module `Claude_eio`): HTTPS client on Eio + cohttp-eio + tls-eio,
  with retries (408/409/429/5xx, honouring `retry-after`), streaming, token
  counting and a tool-use loop.
- **`claude-api.lwt`** (module `Claude_lwt`): the same client API on Lwt +
  cohttp-lwt-unix. Results come back as `_ Lwt.t`, and `on_event` and tool
  handlers return promises. TLS goes through conduit (OpenSSL via `lwt_ssl`, or
  OCaml-TLS if `tls-lwt` is installed).

Shared helpers for writing another backend live in `Claude.Transport`.

### Lwt

```ocaml
open Lwt.Syntax

let main () =
  let client = Claude_lwt.create () in
  let+ r = Claude_lwt.messages_create client (Claude.Request.make [ Claude.Message.user "Hi" ]) in
  match r with
  | Ok resp -> print_endline (Claude.Response.text resp)
  | Error e -> prerr_endline (Claude.Api_error.to_string e)

let () = Lwt_main.run (main ())
```

Lwt examples: `dune exec examples/lwt/{hello_lwt,stream_lwt,tools_lwt}.exe`.

## Setup

```sh
opam install . --deps-only --with-test
export ANTHROPIC_API_KEY=sk-ant-...
dune build && dune test
dune exec examples/hello.exe -- "Explain GADTs in two sentences"
```

## Usage

```ocaml
open Claude

let () =
  Eio_main.run @@ fun env ->
  let client =
    Claude_eio.create ~net:(Eio.Stdenv.net env) ~clock:(Eio.Stdenv.clock env) ()
  in
  let req = Request.make ~system:"Be concise." [ Message.user "Hello!" ] in
  match Claude_eio.messages_create client req with
  | Ok resp -> print_endline (Response.text resp)
  | Error e -> prerr_endline (Api_error.to_string e)
```

**Streaming:**

```ocaml
let on_event = function
  | Stream_event.Content_block_delta { delta = Text_delta s; _ } -> print_string s
  | _ -> ()
in
Claude_eio.messages_stream ~on_event client (Request.make ~max_tokens:64000 msgs)
(* returns the full accumulated Response.t *)
```

**Tool use:** define tools with `Tool.make ~name ~input_schema ()` and let
`Claude_eio.run_tools client ~handler req` run the loop. `handler ~name ~input`
returns `Ok result` or `Error message`. See `examples/tools.ml`.

**Multi-turn:** append `Response.to_message resp` to your history. It replays the
returned blocks verbatim, so thinking signatures and server-tool blocks stay valid.

**Other features:** `~thinking`, `~effort`, `~output_schema` (structured JSON
output), `~cache_control` (prompt caching), `Content.image`/`pdf`/`text_document`,
`Tool.web_search ()`, `Message.system` (mid-conversation instructions),
`Claude_eio.count_tokens`. For anything not modelled, use `~extra` (body fields),
`~betas` (beta headers), `Content.Raw` and `Tool.Builtin`.

## Defaults

- Model: `claude-opus-5-5` (`Model.default`). `max_tokens` is 16000; use streaming for larger values.
- `anthropic-version: 2026-10-21`.
- **Refusal fallbacks**: on models that support it, requests send `fallbacks: "default"`
  (beta `server-side-fallback-2026-07-01`). If a safety classifier declines, the
  API retries on a fallback model in the same call. `resp.model` tells you which
  model answered. Disable it with `~fallbacks:Fallbacks.Disabled`.
- Always check `resp.stop_reason` (`Refusal`, `Max_tokens`, …) before using the content.

Credentials: `ANTHROPIC_API_KEY` (or `ANTHROPIC_AUTH_TOKEN` for OAuth bearer tokens), or pass
`~auth` to `Claude_eio.create`. `ANTHROPIC_BASE_URL` overrides the endpoint.
