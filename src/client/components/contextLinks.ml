open Nes
open Dancelor_client_html
open Dancelor_client_model
open Js_of_ocaml
open Js_of_ocaml_lwt
module PageRouter = Dancelor_common_pageRouter
module Utils = Dancelor_client_utils

let make_context_link_banner ~context ~this_page =
  let parent_href = let open PageRouter in match context with
    | InSearch query -> path_explore (Some query)
  in
  let parent_a ?a:(as_=[]) content =
    a ~a:(a_href parent_href :: as_) content
  in
  div
    ~a:[
      a_class ["context-links"; "context-links-banner"]
    ]
    (
      (
        let open PageRouter in match context with
        | InSearch query ->
          [
            txt "In search for: ";
            parent_a [txt query];
          ]
      )
      @ [
        div ~a:[a_class ["context-links-actions"]] [
          parent_a
            ~a:[
              a_class ["context-links-action"];
              a_title "Return to the parent of this page.";
            ]
            [txt "▴"];
          a
            ~a:[
              a_class ["context-links-action"];
              a_href this_page;
              a_title "Reload the current page without the context. This \
                       will get rid of this banner and of the side links.";
            ]
            [txt "⨉"];
          div ~a:[a_class ["context-links-aligner"]] [];
        ];
        div ~a:[a_class ["context-links-aligner"]] [];
      ]
    )

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
      a_class ["context-links"; (if left then "context-links-left" else "context-links-right")];
    ]
    [
      div ~a:[a_class ["context-links-aligner"]] [];
      let element_repr = [
        [txt Any.(Type.to_string (type_of neighbour))];
        [L.txt @@ Any.name neighbour];
      ] @ (if number_of_others <= 0 then [] else [[txt @@ spf "...and %d more" number_of_others]])
      in
      div (
        (div ~a:[a_class ["context-links-main"]] [txt @@ if left then "‹" else "›"])
        :: List.map (div ~a:[a_class ["context-links-details"]]) element_repr
      );
    ]

let make_and_render ?context ~this_page any_lwt =
  L.div (
    match context with
    | None -> Lwt.return_nil
    | Some ((PageRouter.InSearch query) as context) ->
      let%lwt any = any_lwt in
      (* TODO: Unify with [Explorer.search]. *)
      let threshold = 0.4 in
      let filter = Result.get_ok (Any.Filter.from_string query) in
      let%lwt (total, prev, index, next) = Any.search_context ~threshold filter any in
      Lwt.return @@ List.filter_map Fun.id [
        make_context_link ~context ~left:true ~neighbour:prev ~number_of_others:(index - 1);
        make_context_link ~context ~left:false ~neighbour:next ~number_of_others:(total - index - 2);
        (* The banner must be placed after the side-links so as to be appear on
           top in the HTML rendering. *)
        Some (make_context_link_banner ~context ~this_page);
      ]
  );
