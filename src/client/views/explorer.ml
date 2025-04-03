open Nes
open Common

open Js_of_ocaml
open Model
open Components
open Html

let update_uri input =
  let uri = Endpoints.Page.(href Explore) (Some input) in
  Dom_html.window##.history##replaceState
    "fixme-the-state"
    (Js.string "")
    (Js.some (Js.string uri))

let search slice input =
  let%rlwt filter = Lwt.return (Any.Filter.from_string input) in
  Lwt.map Result.ok @@ Any.search slice filter

(** Generic row showing an emoji on the left and a message on the right. *)
let emoji_row emoji message =
  tr
    [
      td [txt emoji];
      td ~a: [a_colspan 4] [txt message];
    ]

let create ?query () =
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
      ?initial_input: query
      ()
  in
  let title = "Explore" in
  Page.make
    ~title: (S.const title)
    [
      div
        ~a: [a_class ["input-group"; "mb-4"]]
        [
          i ~a: [a_class ["input-group-text"; "bi"; "bi-search"]] [];
          SearchBar.render
            ~id: "explorer-search-bar"
            ~placeholder: "Search for anything (it really is magic!)"
            ~autofocus: true
            ~on_input: update_uri
            search_bar;
          button
            ~a: [
              a_class ["btn"; "btn-primary"];
              a_button_type `Button;
              a_onclick (fun _ ->
                  Lwt.async (fun () ->
                      let search_text = S.value (SearchBar.text search_bar) in
                      (* TODO: On return, add a space and focus the search bar. *)
                      Fun.flip
                        Lwt.map
                        (SearchComplexFiltersDialog.open_ search_text)
                        (Result.iter (fun text -> SearchBar.set_text search_bar text; update_uri text))
                    );
                  false
                );
            ]
            [
              i ~a: [a_class ["bi"; "bi-filter"]] [];
              txt " Filter"
            ];
        ];
      R.div
        (
          Fun.flip S.map (SearchBar.state search_bar) @@ function
          | NoResults -> [div ~a: [a_class ["alert"; "alert-warning"]] [txt "Your search returned no results."]]
          | Errors error -> [div ~a: [a_class ["alert"; "alert-danger"]] [txt error]]
          | StartTyping | ContinueTyping | Results _ -> []
        );
      div
        ~a: [
          R.a_class
            (
              Fun.flip S.map (SearchBar.state search_bar) @@ function
              | Results _ -> []
              | _ -> ["d-none"]
            )
        ]
        [
          Pagination.render ~is_below: false pagination;
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
                      Fun.flip S.map (S.Pair.pair (Pagination.slice pagination) (SearchBar.state search_bar))
                      @@ fun (_, state) ->
                      match state with
                      | Results results ->
                        let context = S.map Endpoints.Page.inSearch @@ SearchBar.text search_bar in
                        List.map Utils.(ResultRow.to_clickable_row % AnyResult.(make_result ~context)) results
                      | _ -> []
                    )
                ];
            ];
          Pagination.render ~is_below: true pagination;
        ]
    ]
