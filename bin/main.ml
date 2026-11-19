(* This file is free software, part of linol. See file "LICENSE" for more information *)

(* Some user code

   The code here is just a placeholder to make this file compile, it is expected
   that users have an implementation of a processing function for input contents.

   Here we expect a few things:
   - a type to represent a state/environment that results from processing an
     input file
   - a function procdessing an input file (given the file contents as a string),
     which return a state/environment
   - a function to extract a list of diagnostics from a state/environment.
     Diagnostics includes all the warnings, errors and messages that the processing
     of a document are expected to be able to return.
*)

module Lsp = Linol.Lsp

let diagnostics _state : Lsp.Types.Diagnostic.t list =
  let range =
    let start = Linol_eio.Position.create ~character:2 ~line:1 in
    let end_ = Linol_eio.Position.create ~character:6 ~line:1 in
    Linol_eio.Range.create ~start ~end_
  in
  [ Lsp.Types.Diagnostic.create ~message:(`String "Hello") ~range () ]

(* Lsp server class

   This is the main point of interaction beetween the code checking documents
   (parsing, typing, etc...), and the code of linol.

   The [Linol_eio.Jsonrpc2.server] class defines a method for each of the action
   that the lsp server receives, such as opening of a document, when a document
   changes, etc.. By default, the method predefined does nothing (or errors out ?),
   so that users only need to override methods that they want the server to
   actually meaningfully interpret and respond to.
*)
class lsp_server ~env ~(sw : Eio.Switch.t) =
  object (self)
    inherit Linol_eio.Jsonrpc2.server as super
    method! config_completion = Some (Linol_eio.CompletionOptions.create ())

    method! config_modify_capabilities capabilities =
      let capabilities = super#config_modify_capabilities capabilities in
      { capabilities with definitionProvider = Some (`Bool true) }

    (* one env per document *)
    val buffers : (Lsp.Types.DocumentUri.t, string) Hashtbl.t =
      Hashtbl.create 32

    method spawn_query_handler f = Linol_eio.spawn ~sw f

    (* We define here a helper method that will:
       - process a document
       - store the state resulting from the processing
       - return the diagnostics from the new state
    *)
    method private _on_doc ~(notify_back : Linol_eio.Jsonrpc2.notify_back)
        (uri : Lsp.Types.DocumentUri.t) (contents : string) =
      Hashtbl.replace buffers uri contents;
      let diags = diagnostics () in
      notify_back#send_diagnostic diags

    (* We now override the [on_notify_doc_did_open] method that will be called
       by the server each time a new document is opened. *)
    method on_notif_doc_did_open ~notify_back d ~content : unit Linol_eio.t =
      self#_on_doc ~notify_back d.uri content

    (* Similarly, we also override the [on_notify_doc_did_change] method that will be called
       by the server each time a new document is opened. *)
    method on_notif_doc_did_change ~notify_back d _c ~old_content:_old
        ~new_content =
      self#_on_doc ~notify_back d.uri new_content

    (* On document closes, we remove the state associated to the file from the global
       hashtable state, to avoid leaking memory. *)
    method on_notif_doc_did_close ~notify_back:_ d : unit Linol_eio.t =
      Hashtbl.remove buffers d.uri;
      ()

    method! on_req_definition ~notify_back:_ ~id:_ ~uri ~pos ~workDoneToken:_
        ~partialResultToken:_ doc_state =
      let prompt =
        Format.sprintf
          {|Hello AI. The user has cursor at position %s and called the "definition" lsp request. The buffer %s has content:

```ocaml
"%s
```

Please output in LSP json format the answer to the query, without anything else in the response. Do not wrap it in Markdown code block as I'm parsing the answer as json directly.|}
          (pos |> Linol_eio.Position.yojson_of_t |> Yojson.Safe.to_string)
          (uri |> Linol_eio.DocumentUri.to_string)
          doc_state.content
      in
      match Clode.request env prompt with
      | Error _ -> None
      | Ok res ->
          Format.eprintf "Answer from model: %s\n%!" res;
          let json = Yojson.Safe.from_string res in
          let res = Linol_eio.Locations.t_of_yojson json in
          Some res

    method config_code_action_provider = `Bool true

    method! on_req_code_action ~notify_back:_ ~id:_
        (c : Linol_eio.CodeActionParams.t) =
      let range = c.range in
      let uri = c.textDocument.uri in
      let text = Hashtbl.find buffers uri in
      let doc =
        let open Linol_eio in
        Lsp.Text_document.make ~position_encoding:positionEncoding
          (DidOpenTextDocumentParams.create
             ~textDocument:
               (TextDocumentItem.create ~languageId:"something" ~uri ~version:0
                  ~text))
      in
      let start, end_ = Lsp.Text_document.absolute_range doc range in
      let extracted_text = String.sub text start (end_ - start) in
      let claude_output =
        let prompt =
          Format.sprintf
            {|Hello AI. The user has triggered the following request: %s.

The buffer has content:

```ocaml
"%s
```

Please output exactly the text that needs to replace the extension node. Do not wrap it in markdown code block.|}
            extracted_text text
        in
        Clode.request env prompt
      in
      match claude_output with
      | Error _ -> None
      | Ok claude_output ->
          let edit =
            Linol_eio.WorkspaceEdit.create
              ~changes:
                [
                  ( uri,
                    [ Linol_eio.TextEdit.create ~newText:claude_output ~range ]
                  );
                ]
              ()
          in
          (Some
             [
               `CodeAction
                 (Linol_eio.CodeAction.create ~edit
                    ~title:"Replace by Claude oupput" ());
             ]
            : Linol_eio.CodeActionResult.t)
  end

(* Main code
   This is the code that creates an instance of the lsp server class
   and runs it as a task. *)
let run () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let s = new lsp_server ~env ~sw in
  let server = Linol_eio.Jsonrpc2.create_stdio ~env s in
  let task () =
    let shutdown () = s#get_status = `ReceivedExit in
    Linol_eio.Jsonrpc2.run ~shutdown server
  in
  match task () with
  | () -> ()
  | exception e ->
      let e = Printexc.to_string e in
      Printf.eprintf "error: %s\n%!" e;
      exit 1

(* Finally, we actually run the server *)
let () = run ()
