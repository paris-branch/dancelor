open Nes
open Html

let search slice input =
  let%rlwt filter = Lwt.return (Model.Any.Filter.from_string input) in
  Lwt.map Result.ok @@ Model.Any.search slice filter

type t = {
  pagination: Pagination.t;
  search_bar: Model.Any.t SearchBar.t;
}
[@@deriving fields]

let make ?initial_input () =
  let (number_of_entries, set_number_of_entries) = S.create None in
  let pagination =
    Pagination.create
      ~entries_per_page: 25
      ~number_of_entries
  in
  let search_bar =
    SearchBar.make
      ~search
      ~slice: (Pagination.slice pagination)
      ~on_number_of_entries: (set_number_of_entries % Option.some)
      ?initial_input
      ()
  in
  {pagination; search_bar}

let render ?on_input ?(attached_buttons = []) t =
  div
    [
      div
        ~a: [a_class ["input-group"; "mb-4"]]
        (
          [
            i ~a: [a_class ["input-group-text"; "bi"; "bi-search"]] [];
            SearchBar.render
              ~id: "explorer-search-bar"
              ~placeholder: "Search for anything (it really is magic!)"
              ~autofocus: true
              ?on_input
              t.search_bar;
          ] @
          attached_buttons
        );
      R.div
        (
          Fun.flip S.map (SearchBar.state t.search_bar) @@ function
          | NoResults -> [div ~a: [a_class ["alert"; "alert-warning"]] [txt "Your search returned no results."]]
          | Errors error -> [div ~a: [a_class ["alert"; "alert-danger"]] [txt error]]
          | StartTyping | ContinueTyping | Results _ -> []
        );
      div
        ~a: [
          R.a_class
            (
              Fun.flip S.map (SearchBar.state t.search_bar) @@ function
              | Results _ -> []
              | _ -> ["d-none"]
            )
        ]
        [
          Pagination.render ~is_below: false t.pagination;
          div
            ~a: [a_class ["table-responsive"]]
            [
              tablex
                ~a: [a_class ["table"; "table-striped"; "table-hover"; "table-borderless"; "my-2"]]
                ~thead: (
                  thead
                    ~a: [a_class ["table-primary"]]
                    [
                      tr [th [txt "Type"]; th [txt "Name"]; th [txt "Kind"]; th [txt "By"];]
                    ];
                )
                ~tfoot: (
                  tfoot
                    ~a: [a_class ["table-primary"]]
                    [
                      tr [th [txt "Type"]; th [txt "Name"]; th [txt "Kind"]; th [txt "By"];]
                    ];
                )
                [
                  R.tbody
                    (
                      Fun.flip S.map (S.Pair.pair (Pagination.slice t.pagination) (SearchBar.state t.search_bar))
                      @@ fun (_, state) ->
                      match state with
                      | Results results ->
                        let context = S.map Common.Endpoints.Page.inSearch @@ SearchBar.text t.search_bar in
                        List.map Utils.(ResultRow.to_clickable_row % AnyResult.(make_result ~context)) results
                      | _ -> []
                    )
                ];
            ];
          Pagination.render ~is_below: true t.pagination;
        ]
    ]
