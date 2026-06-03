open Nes
open Dancelor_common
open Html
open Utils
open Model
open Model_new

let book_page_to_any = function
  | Book.Part _ -> lwt_none
  | Book.Dance (dance, _) ->
    let%lwt dance = Option.get <$> Model.Dance.get dance in
    lwt_some (Any.Dance dance) (* FIXME: a “page” viewer *)
  | Book.Set (set, _) ->
    let%lwt set = Option.get <$> Model.Set.get set in
    lwt_some (Any.Set set)
  | Book.Versions versions_and_params ->
    let%lwt version = (Option.get <%> Model.Version.get) (fst @@ NEList.hd versions_and_params) in
    lwt_some (Any.Version version) (* FIXME: others? fixed by a page viewer *)

(** Given an element and a context, find the total number of elements, the
    previous element, the index of the given element and the next element. *)
let get_neighbours any = function
  | Endpoints.Page.In_search query ->
    (* TODO: Unify with [Explorer.search]. *)
    let%olwt filter = lwt @@ Result.to_option @@ Text_formula.string_to_formula Filter.Any.converter query in
    let%olwt {total; previous_item; index; next_item} = Result.to_option <$> Madge_client.call Endpoints.Api.(route @@ Any Search_context) filter (old_any_to_any_id any) in
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
    let%olwt book = Book.get book in
    let%lwt viewable_content = Monadise_lwt.monadise_1_1 List.filter_map book_page_to_any (Book.contents' book) in
    match List.findi_context (fun i _ -> i = index) viewable_content with
    | None -> lwt_none
    | Some List.{total; previous; index; next; element = _} ->
      lwt_some (total, Option.map old_any_to_any_id previous, index, Option.map old_any_to_any_id next)

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
