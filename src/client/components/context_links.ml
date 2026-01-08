open Nes
open Common
open Html
open Utils
open Model

let book_page_to_any = function
  | Book.Part _ -> None
  | Book.Dance (dance, _) -> Some (Any.Dance dance) (* FIXME: a “page” viewer *)
  | Book.Set (set, _) -> Some (Any.Set set)
  | Book.Versions versions_and_params -> Some (Any.version @@ fst @@ NEList.hd versions_and_params) (* FIXME: others? fixed by a page viewer *)

(** Given an element and a context, find the total number of elements, the
    previous element, the index of the given element and the next element. *)
let get_neighbours any = function
  | Endpoints.Page.In_search query ->
    (* TODO: Unify with [Explorer.search]. *)
    let filter = Result.get_ok (Filter.Any.from_string query) in
    let%lwt (total, previous, index, next) =
      Madge_client.call_exn Endpoints.Api.(route @@ Any Search_context) filter any
    in
    lwt List.{total; previous; index; next; element = any}
  | Endpoints.Page.In_set (set, index) ->
    let%lwt set = Option.get <$> Set.get set in
    let%lwt context = Option.get <$> Set.find_context' index set in
    assert (any = Any.Version context.element);
    lwt @@ List.map_context Any.version context
  | Endpoints.Page.In_book (book, index) ->
    let%lwt book = Option.get <$> Book.get book in
    let%lwt contents = Book.contents' book in
    let viewable_content = List.filter_map book_page_to_any contents in
    lwt @@ Option.get @@ List.findi_context (fun i _ -> i = index) viewable_content

let neighbour_context ~left = function
  | Endpoints.Page.In_search query -> Endpoints.Page.In_search query
  | Endpoints.Page.In_set (id, index) -> Endpoints.Page.In_set (id, index + if left then (-1) else 1)
  | Endpoints.Page.In_book (id, index) -> Endpoints.Page.In_book (id, index + if left then (-1) else 1)

let make_and_render ?context ~this_page any_lwt =
  Option.fold
    context
    ~none: (div [])
    ~some: (fun context ->
      let parent_href =
        let open Endpoints.Page in
        match context with
        | In_search query -> href Explore (Some query)
        | In_set (id, _) -> href_set id
        | In_book (id, _) -> href_book id
      in
      let parent_a content = a ~a: [a_href parent_href] content in
      let context_links_placeholder = [
        div ~a: [a_class ["col-auto"; "text-start"; "p-0"]] [
          Button.make
            ~classes: ["btn-secondary"; "placeholder"]
            ~icon: (Action Move_left)
            ~disabled: (S.const true)
            ();
        ];
        div ~a: [a_class ["col"; "text-center"]] [
          span_placeholder ();
        ];
        div
          ~a: [a_class ["col-auto"; "text-end"; "p-0"]]
          [
            Button.make
              ~classes: ["btn-secondary"; "placeholder"]
              ~icon: (Action Back)
              ~disabled: (S.const true)
              ();
            Button.make
              ~classes: ["btn-warning"; "placeholder"]
              ~icon: (Action Clear)
              ~disabled: (S.const true)
              ();
            Button.make
              ~classes: ["btn-secondary"; "ms-1"; "placeholder"]
              ~icon: (Action Move_right)
              ~disabled: (S.const true)
              ();
          ];
      ]
      in
      let context_links_lwt =
        let%lwt NesList.{total; index; previous; next; _} =
          (fun any -> get_neighbours any context) =<< any_lwt
        in
        lwt [
          div ~a: [a_class ["col-auto"; "text-start"; "p-0"]] [
            Button.make_a
              ~classes: ["btn-secondary"]
              ~icon: (Action Move_left)
              ~disabled: (S.const @@ Option.is_none previous)
              ~tooltip: "Go to the previous element in the context."
              ~href: (S.const @@ Option.fold ~none: "" ~some: (Endpoints.Page.href_any_full ~context: (neighbour_context ~left: true context)) previous)
              ();
          ];
          div ~a: [a_class ["col"; "text-center"]] [
            txt (spf "%d of %d in " (index + 1) total);
            with_span_placeholder
              (
                let open Endpoints.Page in
                match context with
                | In_search "" ->
                  lwt [txt "all the entries"]
                | In_search query ->
                  lwt [txt "search for: "; parent_a [txt query]]
                | In_set (id, _) ->
                  let%lwt name = Set.name' % Option.get <$> Set.get id in
                  lwt [txt "set: "; parent_a [txt @@ NEString.to_string name]]
                | In_book (id, _) ->
                  let%lwt name = Book.title' % Option.get <$> Book.get id in
                  lwt [txt "book: "; parent_a [txt @@ NEString.to_string name]]
              );
          ];
          div
            ~a: [a_class ["col-auto"; "text-end"; "p-0"]]
            [
              Button.make_a
                ~classes: ["btn-secondary"]
                ~icon: (Action Back)
                ~tooltip: "Go back to the parent page, be it a search, a set, \
                           or anything else."
                ~href: (S.const parent_href)
                ();
              Button.make_a
                ~classes: ["btn-warning"]
                ~icon: (Action Clear)
                ~tooltip: "Reload the current page without the context. This will get \
                           rid of this banner and of the side links."
                ~href: (S.const this_page)
                ();
              Button.make_a
                ~classes: ["btn-secondary"; "ms-1"]
                ~icon: (Action Move_right)
                ~disabled: (S.const @@ Option.is_none next)
                ~tooltip: "Go to the next element in the context."
                ~href: (S.const @@ Option.fold ~none: "" ~some: (Endpoints.Page.href_any_full ~context: (neighbour_context ~left: false context)) next)
                ();
            ];
        ]
      in
      div
        [
          div
            ~a: [a_class ["bg-secondary-subtle"; "mt-n2"; (* keep in sync with header's margin *) "mb-2"; "opacity-75"]]
            [
              div ~a: [a_class ["container-md"]] [
                R.div ~a: [a_class ["row"; "m-0"; "align-items-center"]] (
                  S.from' context_links_placeholder context_links_lwt
                );
              ];
            ]
        ]
    )
