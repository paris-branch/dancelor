open Nes
open Dancelor_common
open Search_new
open Html
open Utils
open Model
open Model_new

let book_page_to_any = function
  | Book_view.Part _ -> None
  | Book_view.Dance (dance, _) -> Some (Any_row.Dance dance) (* FIXME: a “page” viewer *)
  | Book_view.Set (set, _) -> Some (Any_row.Set set)
  | Book_view.Versions versions_and_params ->
    let version = fst @@ List.hd versions_and_params in
    Some (Any_row.Version version) (* FIXME: others? fixed by a page viewer *)

(** Given an element and a context, find the total number of elements, the
    previous element, the index of the given element and the next element. *)
let get_neighbours any = function
  | Endpoints.Page.In_search query ->
    (* TODO: Unify with [Explorer.search]. *)
    let%olwt query = lwt @@ Result.to_option @@ Any_query.parse query in
    let%olwt {total; previous_item; index; next_item} = Result.to_option <$> Madge_client.call Endpoints.Api.(route @@ Any Search_context) query (old_any_to_any_id any) in
    lwt_some (total, previous_item, index, next_item)
  | Endpoints.Page.In_set (set, index) ->
    let%olwt set = Set.get set in
    let%olwt context = lwt @@ Set.find_context' index set in
    let%olwt element = Model.Version.get context.element in
    let context = List.map_context (const element) context in
    assert (any = Any.Version context.element);
    let List.{total; previous; index; next; element = _} = List.map_context Any.version context in
    lwt_some (total, Option.map old_any_to_any_id previous, index, Option.map old_any_to_any_id next)
  | Endpoints.Page.In_book (book, index) ->
    let%olwt book = madge_call_or_option (Book Get_view) book in
    let viewable_content = List.filter_map book_page_to_any book.content in
    match List.findi_context (fun i _ -> i = index) viewable_content with
    | None -> lwt_none
    | Some List.{total; previous; index; next; element = _} ->
      lwt_some (total, Option.map Any_row.to_id previous, index, Option.map Any_row.to_id next)

let neighbour_context ~left = function
  | Endpoints.Page.In_search query -> Endpoints.Page.In_search query
  | Endpoints.Page.In_set (id, index) -> Endpoints.Page.In_set (id, index + if left then (-1) else 1)
  | Endpoints.Page.In_book (id, index) -> Endpoints.Page.In_book (id, index + if left then (-1) else 1)

let make_and_render ?context ~this_page any_lwt =
  Option.fold
    context
    ~none: (div [])
    ~some: (fun context ->
      let neighbours_lwt = flip get_neighbours context =<< any_lwt in
      let parent_href =
        let open Endpoints.Page in
        match context with
        | In_search query ->
          S.bind (S.from_lwt None (Lwt.map (Option.map (fun (_, _, index, _) -> index)) neighbours_lwt)) @@ fun index ->
          let page = Option.map (fun index -> 1 + index / Search.entries_per_page) index in
          (* NOTE: sync with search.ml *)
          S.const @@ href Explore (Some query) page
        | In_set (id, _) -> S.const @@ href_set id
        | In_book (id, _) -> S.const @@ href_book id
      in
      let parent_a content = a ~a: [R.a_href parent_href] content in
      let context_links = [
        div ~a: [a_class ["col"; "ps-0"]] [
          with_span_placeholder
            (
              uncurry List.cons
              <$> Lwt.both
                  (
                    Lwt.flip_map neighbours_lwt @@ function
                      | None -> txt "?? of ?? in "
                      | Some (total, _, index, _) -> txtf "%d of %d in " (index + 1) total
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
                      let%lwt name = Book.name' % Option.get <$> Book.get id in
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
              ~href: parent_href
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
              S.from_lwt None @@
              Lwt.flip_map neighbours_lwt @@
              Option.map @@ fun (_, previous, _, next) ->
              [
                Button.make_a
                  ~classes: ["btn-secondary"]
                  ~icon: (Action Move_left)
                  ~disabled: (S.const @@ Option.is_none previous)
                  ~tooltip: "Go to the previous element in the context."
                  ~href: (S.const @@ Option.fold ~none: Uri.empty ~some: (Endpoints.Page.href_any_full_new ~context: (neighbour_context ~left: true context)) previous)
                  ();
                Button.make_a
                  ~classes: ["btn-secondary"]
                  ~icon: (Action Move_right)
                  ~disabled: (S.const @@ Option.is_none next)
                  ~tooltip: "Go to the next element in the context."
                  ~href: (S.const @@ Option.fold ~none: Uri.empty ~some: (Endpoints.Page.href_any_full_new ~context: (neighbour_context ~left: false context)) next)
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

let make_and_render_new ?context ~this_page (any_id : Any_id.t) =
  make_and_render ?context ~this_page (any_id_to_old_any any_id)
