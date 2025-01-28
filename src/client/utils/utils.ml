open Nes
open Js_of_ocaml

module Endpoints = Dancelor_common.Endpoints

module AnyResult = AnyResult
module ResultRow = ResultRow

let rec is_child_of : 'a 'b. ((#Dom.node as 'a) Js.t) -> ((#Dom.node as 'b) Js.t) -> bool = fun c p ->
  ((c :> Dom.node Js.t) = (p :> Dom.node Js.t))
  || (
    Js.Opt.case
      c##.parentNode
      (Fun.const false)
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
               let count_lwt = Lwt.bind filter_lwt Model.Any.count in
               li
                 ~a: [
                   L.a_class
                     (
                       Fun.flip Lwt.map count_lwt @@ function
                       | 0 -> ["disabled"]
                       | _ -> []
                     );
                 ]
                 [
                   a
                     ~a: [
                       L.a_href
                         (
                           Lwt.map
                             (Endpoints.Page.(href Explore) % Option.some % Model.Any.Filter.to_string)
                             filter_lwt
                         );
                     ]
                     [txt text];
                   L.txt (Lwt.map (spf " (%d)") count_lwt);
                   txt ",";
                 ]
            )
            links
        );
    ]

let quick_explorer_links' model_lwt links =
  quick_explorer_links @@ List.map (fun (text, mk_filter) -> (text, Lwt.map mk_filter model_lwt)) links
