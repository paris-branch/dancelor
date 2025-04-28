open Nes
open Common

open Html
open Model
open Js_of_ocaml
open Js_of_ocaml_lwt

let book_page_to_any = function
  | Book.Set (set, _) -> Any.Set set
  | Version (version, _) -> Any.Version version
  | InlineSet _ -> assert false

(** Given an element and a context, find the total number of elements, the
    previous element, the index of the given element and the next element. *)
let get_neighbours any = function
  | Endpoints.Page.InSearch query ->
    (* TODO: Unify with [Explorer.search]. *)
    let filter = Result.get_ok (Any.Filter.from_string query) in
    let%lwt (total, previous, index, next) =
      Madge_client.call Endpoints.Api.(route @@ Any SearchContext) filter any
    in
    Lwt.return List.{total; previous; index; next; element = any}
  | Endpoints.Page.InSet (set, index) ->
    let%lwt set = Set.get set in
    let%lwt context = Lwt.map Option.get @@ Set.find_context index set in
    assert (any = Any.Version context.element);
    Lwt.return @@ List.map_context Any.version context
  | Endpoints.Page.InBook (book, index) ->
    let%lwt book = Book.get book in
    let%lwt context =
      Lwt.map (List.map_context book_page_to_any % Option.get) @@
        Book.find_context_no_inline index book
    in
    Lwt.return context

let make_context_link_banner ~context ~this_page =
  let parent_href =
    let open Endpoints.Page in
    match context with
    | InSearch query -> href Explore (Some query)
    | InSet (slug, _) -> href_set slug
    | InBook (slug, _) -> href_book slug
  in
  let parent_a ?a: (as_ = []) content =
    a ~a: (a_href parent_href :: as_) content
  in
  div
    ~a: [a_class ["bg-secondary-subtle"; "p-2"; "mt-n2"; (* keep in sync with header's margin *) "mb-2"; "opacity-75"]]
    [
      div
        ~a: [a_class ["container"; "d-flex"; "justify-content-between"]]
        [
          div
            (
              (
                let open Endpoints.Page in
                match context with
                | InSearch query ->
                  [
                    txt "In search for: ";
                    parent_a [txt query];
                  ]
                | InSet (slug, _) ->
                  [
                    txt "In set: ";
                    parent_a [L.txt (Lwt.map Set.name @@ Set.get slug)];
                  ]
                | InBook (slug, _) ->
                  [
                    txt "In book: ";
                    parent_a [L.txt (Lwt.map Book.title @@ Book.get slug)];
                  ]
              )
            );
          div
            [
              parent_a
                ~a: [a_title "Return to the parent of this page."]
                [i ~a: [a_class ["bi"; "bi-arrow-counterclockwise"]] []];
              a
                ~a: [
                  a_href this_page;
                  a_title
                    "Reload the current page without the context. This \
                     will get rid of this banner and of the side links.";
                ]
                [i ~a: [a_class ["bi"; "bi-x-lg"]] []];
            ];
        ];
    ]

let register_body_keydown_listener f =
  let rec body_keydown_listener () =
    Lwt_js_events.keydowns Dom_html.document##.body @@ fun ev _thread ->
    f ev;
    body_keydown_listener ()
  in
  Lwt.async body_keydown_listener

let neighbour_context ~left = function
  | Endpoints.Page.InSearch query -> Endpoints.Page.InSearch query
  | Endpoints.Page.InSet (slug, index) -> Endpoints.Page.InSet (slug, index + if left then (-1) else 1)
  | Endpoints.Page.InBook (slug, index) -> Endpoints.Page.InBook (slug, index + if left then (-1) else 1)

let make_context_link ~context ~left ~neighbour ~number_of_others =
  Fun.flip Option.map neighbour @@ fun neighbour ->
  let href = Endpoints.Page.href_any ~context: (neighbour_context ~left context) neighbour in
  register_body_keydown_listener (fun ev ->
    if ev##.keyCode = (if left then 37 else 39) then
      Dom_html.window##.location##.href := Js.string href
  );
  a
    ~a: [
      a_href href;
      a_class [
        "bg-secondary-subtle";
        "opacity-0";
        "hover-opacity-50";
        "position-fixed";
        "top-50";
        (if left then "start-0" else "end-0");
        "translate-middle-y";
        "text-center";
        "p-2";
      ];
    ]
    [
      div
        ~a: [
        ]
        [
          let element_repr =
            [[txt Any.(Type.to_string (type_of neighbour))];
            [L.txt @@ Any.name neighbour];
            ] @
              (if number_of_others <= 0 then [] else [[txt @@ spf "...and %d more" number_of_others]])
          in
          div
            (
              (
                i
                  ~a: [a_class ["fs-1"; "bi"; ("bi-caret-" ^ (if left then "left" else "right") ^ "-fill")]]
                  []
              ) :: List.map div element_repr
            );
        ];
    ]

let make_and_render ?context ~this_page any_lwt =
  match context with
  | None -> div []
  | Some context ->
    L.div
      (
        let%lwt any = any_lwt in
        let%lwt {total; previous; index; next; _} = get_neighbours any context in
        Lwt.return @@
          List.filter_map
            Fun.id
            [
              make_context_link ~context ~left: true ~neighbour: previous ~number_of_others: (index - 1);
              make_context_link ~context ~left: false ~neighbour: next ~number_of_others: (total - index - 2);
              (* The banner must be placed after the side-links so as to be appear on
                 top in the HTML rendering. *)
              Some (make_context_link_banner ~context ~this_page);
            ]
      );
