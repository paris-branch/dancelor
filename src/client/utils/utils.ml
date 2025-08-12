open Nes
open Common

open Js_of_ocaml

module AnyResult = AnyResult
module ResultRow = ResultRow

let write_to_clipboard = Clipboard.write_to_clipboard

let rec is_child_of : 'a 'b. ((#Dom.node as 'a) Js.t) -> ((#Dom.node as 'b) Js.t) -> bool = fun c p ->
  ((c :> Dom.node Js.t) = (p :> Dom.node Js.t))
  || (
    Js.Opt.case
      c##.parentNode
      (const false)
      (fun p' -> is_child_of p' p)
  )

let is_input : 'a. (#Dom.element as 'a) Js.t -> bool = fun n ->
  let tag = String.lowercase_ascii (Js.to_string n##.tagName) in
  tag = "input" || tag = "textarea"

let add_target_event_listener n ev f =
  let open Dom_html in
  ignore @@
    addEventListener
      n
      ev
      (
        handler @@ fun event ->
        Js.Opt.case event##.target (fun () -> Js._true) (f event)
      )
      Js._false (* default: run in bubbling phase *)

let quick_explorer_links links =
  let open Html in
  div
    ~a: [a_class ["section"]]
    [
      txt "Quick links to:";
      ul
        ~a: [a_class ["bullet-list"]]
        (
          List.map
            (fun (text, filter_lwt) ->
              let count_lwt =
                fst
                <$> (
                    Madge_client.call_exn Endpoints.Api.(route @@ Any Search) Slice.nothing
                    =<< filter_lwt
                  )
              in
              li
                ~a: [
                  R.a_class
                    (
                      S.from' [] @@
                      flip Lwt.map count_lwt @@ function
                      | 0 -> ["disabled"]
                      | _ -> []
                    );
                ]
                [
                  a
                    ~a: [
                      R.a_href
                        (
                          S.from' "" (
                            (Endpoints.Page.(href Explore) % some % Filter.Any.to_string)
                            <$> filter_lwt
                          )
                        );
                    ]
                    [txt text];
                  R.txt (S.from' "" (spf " (%d)" <$> count_lwt));
                  txt ",";
                ]
            )
            links
        );
    ]

let quick_explorer_links' model_lwt links =
  quick_explorer_links @@ List.map (fun (text, mk_filter) -> (text, mk_filter <$> model_lwt)) links

let href_any_for_sharing any =
  let current = Uri.of_string (Js.to_string Dom_html.window##.location##.href) in
  let path = Endpoints.Page.(href Any) @@ Entry.id @@ ModelBuilder.Core.Any.to_entry any in
  Uri.to_string @@ Uri.with_query (Uri.with_path current path) []
