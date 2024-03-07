open Nes
open Dancelor_client_html
open Dancelor_client_model
open Js_of_ocaml
open Js_of_ocaml_lwt
module PageRouter = Dancelor_common_pageRouter
module Utils = Dancelor_client_utils

let register_body_keydown_listener f =
  let rec body_keydown_listener () =
    Lwt_js_events.keydowns Dom_html.document##.body @@ fun ev _thread ->
    f ev;
    body_keydown_listener ()
  in
  Lwt.async body_keydown_listener

let make_context_link ~context ~left ~neighbour ~number_of_others =
  Fun.flip Option.map neighbour @@ fun neighbour ->
  let href = PageRouter.path_any ~context neighbour in
  register_body_keydown_listener (fun ev ->
      if ev##.keyCode = (if left then 37 else 39) then
        Dom_html.window##.location##.href := Js.string href);
  a
    ~a:[
      a_href href;
      a_class ["context-link"; (if left then "context-link-left" else "context-link-right")];
    ]
    [
      div ~a:[a_class ["context-link-aligner"]] [];
      let context_repr = match context with
        | InSearch query -> [[txt "In search for"]; [txt query]]
      in
      let element_repr = [
        [txt Any.(Type.to_string (type_of neighbour))];
        [L.txt @@ Any.name neighbour];
      ] @ (if number_of_others <= 0 then [] else [[txt @@ spf "...and %d more" number_of_others]])
      in
      div (
        List.map (div ~a:[a_class ["context-link-detail"]]) context_repr
        @ [div ~a:[a_class ["context-link-main"]] [txt @@ if left then "‹" else "›"]]
        @ List.map (div ~a:[a_class ["context-link-detail"]]) element_repr
      );
    ]

let make_and_render ?context ~search any_lwt =
  L.div (
    match context with
    | None -> Lwt.return_nil
    | Some ((PageRouter.InSearch query) as context) ->
      let%lwt (total, scores) = Lwt.map Result.get_ok (search query) in
      let scores = List.map Score.value scores in
      let%lwt any = any_lwt in
      let (prev, i, _, next) = Option.get @@ List.findi_context (Any.equal any) scores in
      Lwt.return @@ List.filter_map Fun.id [
        make_context_link ~context ~left:true ~neighbour:prev ~number_of_others:(i - 1);
        make_context_link ~context ~left:false ~neighbour:next ~number_of_others:(total - i - 2);
      ]
  );
