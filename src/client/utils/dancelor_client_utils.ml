open Nes
open Js_of_ocaml
module Model = Dancelor_client_model
module PageRouter = Dancelor_common_pageRouter

module AnyResult = AnyResult
module ResultRow = ResultRow

let rec is_child_of : 'a 'b. ((#Dom.node as 'a) Js.t) -> ((#Dom.node as 'b) Js.t) -> bool =
  fun c p ->
  ((c :> Dom.node Js.t) = (p :> Dom.node Js.t)) ||
  (Js.Opt.case c##.parentNode
     (Fun.const false)
     (fun p' -> is_child_of p' p))

let is_input : 'a. (#Dom.element as 'a) Js.t -> bool =
  fun n ->
  let tag = String.lowercase_ascii (Js.to_string n##.tagName) in
  tag = "input" || tag = "textarea"

let add_target_event_listener n ev f =
  let open Dom_html in
  ignore @@
  addEventListener n ev
    (handler @@ fun event ->
     Js.Opt.case event##.target (fun () -> Js._true) (f event))
    Js._false (* default: run in bubbling phase *)

let quick_explorer_links model_lwt links =
  let open Dancelor_client_html in
  div ~a:[a_class ["section"]] [
    txt "Quick links to:";
    ul ~a:[a_class ["bullet-list"]] (
      List.map
        (fun (text, mk_filter) ->
           let filter_lwt = Lwt.map mk_filter model_lwt in
           li [
             a ~a:[L.a_href @@ Lwt.map (PageRouter.path_explore % Option.some % Model.Any.Filter.to_string) filter_lwt]
               [txt text];
             L.txt (
               let%lwt filter = filter_lwt in
               let%lwt n = Model.Any.count filter in
               Lwt.return @@ spf " (%d)" n
             );
             txt ",";
           ]
        )
        links
    );
  ]
