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
    let%olwt filter = lwt @@ Result.to_option @@ Filter.Any.from_string query in
    let%olwt (total, previous, index, next) = Result.to_option <$> Madge_client.call Endpoints.Api.(route @@ Any Search_context) filter any in
    lwt_some List.{total; previous; index; next; element = any}
  | Endpoints.Page.In_set (set, index) ->
    let%olwt set = Set.get set in
    let%olwt context = Set.find_context' index set in
    assert (any = Any.Version context.element);
    lwt_some @@ List.map_context Any.version context
  | Endpoints.Page.In_book (book, index) ->
    let%olwt book = Book.get book in
    let%lwt contents = Book.contents' book in
    let viewable_content = List.filter_map book_page_to_any contents in
    lwt @@ List.findi_context (fun i _ -> i = index) viewable_content

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
      let neighbours_lwt = flip get_neighbours context =<< any_lwt in
      let context_links = [
        div ~a: [a_class ["col"; "ps-0"]] [
          with_span_placeholder
            (
              uncurry List.cons
              <$> Lwt.both
                  (
                    Lwt.flip_map neighbours_lwt @@ function
                      | None -> txt "?? of ?? in "
                      | Some List.{total; index; _} -> txtf "%d of %d in " (index + 1) total
                  )
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
                  )
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
            R.div ~a: [a_class ["d-inline-block"; "ms-1"]] (
              S.map
                (
                  Option.value
                    ~default: [
                      Button.make
                        ~classes: ["btn-secondary"; "placeholder"]
                        ~icon: (Action Move_left)
                        ~disabled: (S.const true)
                        ();
                      Button.make
                        ~classes: ["btn-secondary"; "placeholder"]
                        ~icon: (Action Move_right)
                        ~disabled: (S.const true)
                        ();
                    ]
                ) @@
              S.from' None @@
              Lwt.flip_map neighbours_lwt @@
              Option.map @@ fun List.{previous; next; _} ->
              [
                Button.make_a
                  ~classes: ["btn-secondary"]
                  ~icon: (Action Move_left)
                  ~disabled: (S.const @@ Option.is_none previous)
                  ~tooltip: "Go to the previous element in the context."
                  ~href: (S.const @@ Option.fold ~none: Uri.empty ~some: (Endpoints.Page.href_any_full ~context: (neighbour_context ~left: true context)) previous)
                  ();
                Button.make_a
                  ~classes: ["btn-secondary"]
                  ~icon: (Action Move_right)
                  ~disabled: (S.const @@ Option.is_none next)
                  ~tooltip: "Go to the next element in the context."
                  ~href: (S.const @@ Option.fold ~none: Uri.empty ~some: (Endpoints.Page.href_any_full ~context: (neighbour_context ~left: false context)) next)
                  ();
              ]
            );
          ];
      ]
      in
      div
        [
          div
            ~a: [a_class ["bg-secondary-subtle"; "mt-n2"; (* keep in sync with header's margin *) "mb-2"; "opacity-75"]]
            [
              div ~a: [a_class ["container-md"]] [
                div ~a: [a_class ["row"; "m-0"; "align-items-center"]] (context_links);
              ];
            ]
        ]
    )
