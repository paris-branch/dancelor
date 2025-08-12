open Nes
open Common

open Html
open Model

let book_page_to_any = function
  | Book.Set (set, _) -> Any.Set set
  | Version (version, _) -> Any.Version version
  | InlineSet _ -> assert false

(** Given an element and a context, find the total number of elements, the
    previous element, the index of the given element and the next element. *)
let get_neighbours any = function
  | Endpoints.Page.InSearch query ->
    (* TODO: Unify with [Explorer.search]. *)
    let filter = Result.get_ok (Filter.Any.from_string query) in
    let%lwt (total, previous, index, next) =
      Madge_client.call_exn Endpoints.Api.(route @@ Any SearchContext) filter any
    in
    lwt List.{total; previous; index; next; element = any}
  | Endpoints.Page.InSet (set, index) ->
    let%lwt set = Option.get <$> Set.get set in
    let%lwt context = Option.get <$> Set.find_context' index set in
    assert (any = Any.Version context.element);
    lwt @@ List.map_context Any.version context
  | Endpoints.Page.InBook (book, index) ->
    let%lwt book = Option.get <$> Book.get book in
    let%lwt context =
      List.map_context book_page_to_any
      <$> (Option.get <$> Book.find_context_no_inline' index book)
    in
    lwt context

let neighbour_context ~left = function
  | Endpoints.Page.InSearch query -> Endpoints.Page.InSearch query
  | Endpoints.Page.InSet (id, index) -> Endpoints.Page.InSet (id, index + if left then (-1) else 1)
  | Endpoints.Page.InBook (id, index) -> Endpoints.Page.InBook (id, index + if left then (-1) else 1)

let make_and_render ?context ~this_page any_lwt =
  Option.fold
    context
    ~none: (div [])
    ~some: (fun context ->
      let parent_href =
        let open Endpoints.Page in
        match context with
        | InSearch query -> href Explore (Some query)
        | InSet (id, _) -> href_set id
        | InBook (id, _) -> href_book id
      in
      let parent_a content = a ~a: [a_href parent_href] content in
      R.div (
        S.from' [] @@
          let%lwt any = any_lwt in
          let%lwt {total; previous; index; next; _} = get_neighbours any context in
          ignore total;
          ignore index;
          lwt [
            div
              ~a: [a_class ["bg-secondary-subtle"; "mt-n2"; (* keep in sync with header's margin *) "mb-2"; "opacity-75"]]
              [
                div
                  ~a: [a_class ["container-md"]]
                  [
                    div ~a: [a_class ["row"; "m-0"; "align-items-center"]] [
                      div ~a: [a_class ["col-auto"; "text-start"; "p-0"]] [
                        Utils.Button.make_a
                          ~classes: ["btn-secondary"]
                          ~icon: "arrow-left"
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
                            | InSearch "" ->
                              lwt [txt "all the entries"]
                            | InSearch query ->
                              lwt [txt "search for: "; parent_a [txt query]]
                            | InSet (id, _) ->
                              let%lwt name = Set.name' % Option.get <$> Set.get id in
                              lwt [txt "set: "; parent_a [txt name]]
                            | InBook (id, _) ->
                              let%lwt name = Book.title' % Option.get <$> Book.get id in
                              lwt [txt "book: "; parent_a [txt name]]
                          );
                      ];
                      div
                        ~a: [a_class ["col-auto"; "text-end"; "p-0"]]
                        [
                          Utils.Button.make_a
                            ~classes: ["btn-secondary"]
                            ~icon: "arrow-counterclockwise"
                            ~tooltip: "Go back to the parent page, be it a search, a set, \
                             or anything else."
                            ~href: (S.const parent_href)
                            ();
                          Utils.Button.make_a
                            ~classes: ["btn-warning"]
                            ~icon: "eraser"
                            ~tooltip: "Reload the current page without the context. This will get \
                             rid of this banner and of the side links."
                            ~href: (S.const this_page)
                            ();
                          Utils.Button.make_a
                            ~classes: ["btn-secondary"; "ms-1"]
                            ~icon: "arrow-right"
                            ~disabled: (S.const @@ Option.is_none next)
                            ~tooltip: "Go to the next element in the context."
                            ~href: (S.const @@ Option.fold ~none: "" ~some: (Endpoints.Page.href_any_full ~context: (neighbour_context ~left: false context)) next)
                            ();
                        ];
                    ];
                  ];
              ]
          ]
      )
    )
