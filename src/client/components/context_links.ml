open Nes
open Dancelor_common
open Search_new
open Html
open Utils
open Model
open Model_new

let no_context_links () = div []

let make_and_render_gen ~parent_href ~this_page ~index_total_category_name_lwt ~previous_next_lwt =
  let parent_a content = a ~a: [R.a_href parent_href] content in
  let context_links = [
    div ~a: [a_class ["col"; "ps-0"; "text-truncate"]] [
      with_span_placeholder
        (
          let%lwt (index, total, category, name) = index_total_category_name_lwt in
          lwt @@
            (
              txtf
                "%s of %s in %s"
                (Option.fold index ~some: (string_of_int % ((+) 1)) ~none: "??")
                (Option.fold total ~some: string_of_int ~none: "??")
                category;
            ) :: (
              match name with
              | None -> []
              | Some name -> [txtf ": "; parent_a [txt name]]
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
          ~disabled: (S.const @@ Option.is_none this_page)
          ~href: (S.const @@ Option.value this_page ~default: Uri.empty)
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
          Lwt.flip_map previous_next_lwt @@
          Option.map @@ fun (previous, next) ->
          [
            Button.make_a
              ~classes: ["btn-secondary"]
              ~icon: (Action Move_left)
              ~disabled: (S.const @@ Option.is_none previous)
              ~tooltip: "Go to the previous element in the context."
              ~href: (S.const @@ Option.value previous ~default: Uri.empty)
              ();
            Button.make_a
              ~classes: ["btn-secondary"]
              ~icon: (Action Move_right)
              ~disabled: (S.const @@ Option.is_none next)
              ~tooltip: "Go to the next element in the context."
              ~href: (S.const @@ Option.value next ~default: Uri.empty)
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

let neighbour_context ~left = function
  | Endpoints.Page.In_search query -> Endpoints.Page.In_search query
  | Endpoints.Page.In_set (id, index) -> Endpoints.Page.In_set (id, index + if left then (-1) else 1)

(** Version specialised for an {!Endpoints.Page.context}. *)
let make_and_render ?context ~this_page any_lwt =
  Option.fold
    context
    ~none: (no_context_links ())
    ~some: (fun context ->
      let neighbours_lwt = flip get_neighbours context =<< any_lwt in
      let parent_href =
        let open Endpoints.Page in
        match context with
        | In_search query ->
          S.bind (S.from_lwt None (Lwt.map (Option.map (fun (_, _, index, _) -> index)) neighbours_lwt)) @@ fun index ->
          let page = Option.map (fun index -> 1 + index / Search.entries_per_page) index in
          (* NOTE: sync with search.ml *)
          S.const @@ href Explore query (Option.value page ~default: 1)
        | In_set (id, _) -> S.const @@ href_set id
      in
      let index_total_category_name_lwt =
        let%lwt (index, total) =
          Lwt.map
            (function
              | None -> (None, None)
              | Some (total, _, index, _) -> (Some index, Some total)
            )
            neighbours_lwt
        in
        let%lwt (category, name) =
          match context with
          | In_search "" -> lwt ("all the entries", None)
          | In_search query -> lwt ("search for", Some query)
          | In_set (id, _) ->
            let%lwt name = Set.name' % Option.get <$> Set.get id in
            lwt ("set", Some (NEString.to_string name))
        in
        lwt (index, total, category, name)
      in
      let previous_next_lwt =
        Lwt.map
          (
            Option.map @@ fun (_, previous, _, next) ->
            (
              Option.map (Endpoints.Page.href_any_full_new ~context: (neighbour_context ~left: true context)) previous,
              Option.map (Endpoints.Page.href_any_full_new ~context: (neighbour_context ~left: false context)) next
            )
          )
          neighbours_lwt
      in
      make_and_render_gen ~parent_href ~this_page: (Some this_page) ~index_total_category_name_lwt ~previous_next_lwt
    )

let make_and_render_new ?context ~this_page (any_id : Any_id.t) =
  make_and_render ?context ~this_page (any_id_to_old_any any_id)

let make_and_render_book ~this_page (book : Book_view.t) pageno =
  let parent_href = S.const @@ Endpoints.Page.href_book book.id in
  let total = List.length book.content in
  let index_total_category_name_lwt =
    lwt (Some pageno, Some total, "book", Some book.name)
  in
  let previous_next_lwt =
    let mk pageno =
      if pageno < 0 || pageno > total - 1 then None
      else
        some @@ Endpoints.Page.(href @@ Book Preview) book.id pageno
    in
    lwt_some (mk (pageno - 1), mk (pageno + 1))
  in
  make_and_render_gen ~parent_href ~this_page ~index_total_category_name_lwt ~previous_next_lwt
