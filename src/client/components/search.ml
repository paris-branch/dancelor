open Nes
open Html

module Search = struct
  type 'p pagination_mode =
    | Pagination of 'p
    | FixedSlice of Slice.t

  let slice = function
    | Pagination p -> Pagination.slice p
    | FixedSlice s -> S.const s

  type 'result t = {
    pagination: Pagination.t pagination_mode;
    search_bar: 'result SearchBar.t;
    min_characters: int;
  }
  [@@deriving fields]

  let make
      ?initial_input
      ~search
      ~pagination_mode
      ?(min_characters = 0)
      ?on_input
      ?on_enter
      ()
    =
    let (number_of_entries, set_number_of_entries) = S.create None in
    let pagination =
      match pagination_mode with
      | Pagination() -> Pagination (Pagination.create ~entries_per_page: 25 ~number_of_entries)
      | FixedSlice slice -> FixedSlice slice
    in
    let search_bar =
      SearchBar.make
        ~search
        ~slice: (slice pagination)
        ~on_number_of_entries: (set_number_of_entries % some)
        ~min_characters
        ?initial_input
        ~placeholder: "Search for anything (it really is magic!)"
        ?on_input
        ?on_enter
        ()
    in
      {pagination; search_bar; min_characters}

  let render
      ~make_result
      ?(attached_buttons = [])
      ?(show_table_headers = true)
      t
    =
    div
      [
        (
          (* Alert for when the user is not connected. *)
          R.div
            (
              S.from' [] @@
                match%lwt Madge_client.call_exn Common.Endpoints.Api.(route @@ User Status) with
                | Some _ -> lwt_nil
                | None ->
                  lwt [
                    div ~a: [a_class ["alert"; "alert-info"]; a_role ["alert"]] [
                      txt "You are not connected, and therefore are only seeing public items.";
                    ];
                  ]
            )
        );
        (
          (* The search bar. *)
          div
            ~a: [a_class ["input-group"; "mb-3"]]
            (
              [i ~a: [a_class ["input-group-text"; "bi"; "bi-search"]] [];
              SearchBar.html t.search_bar;
              ] @
                attached_buttons
            )
        );
        (
          (* Info or alert box, eg. to inform that one needs to type. *)
          R.div
            (
              flip S.map (SearchBar.state t.search_bar) @@ function
                | NoResults -> [div ~a: [a_class ["alert"; "alert-warning"]] [txt "Your search returned no results."]]
                | Errors error -> [div ~a: [a_class ["alert"; "alert-danger"]] [txt error]]
                | StartTyping -> [div ~a: [a_class ["alert"; "alert-info"]] [txt "Start typing to search."]]
                | ContinueTyping -> [div ~a: [a_class ["alert"; "alert-info"]] [txt (spf "Type at least %s characters." (Int.to_english_string t.min_characters))]]
                | _ -> []
            )
        );
        (
          (* The results table *)
          div
            ~a: [
              R.a_class
                (
                  flip S.map (SearchBar.state t.search_bar) @@ function
                    | Results _ when show_table_headers -> ["my-4"]
                    | Results _ -> []
                    | _ -> ["d-none"]
                )
            ]
            [
              (
                match t.pagination with
                | Pagination p -> Pagination.render ~is_below: false p
                | FixedSlice _ -> div []
              );
              div
                ~a: [a_class ["table-responsive"]]
                [
                  tablex
                    ~a: [a_class ["table"; "table-striped"; "table-hover"; "table-borderless"; "my-1"]]
                    ?thead: (
                      if show_table_headers then
                        Some
                          (
                            thead
                              ~a: [a_class ["table-primary"]]
                              [
                                tr [th [span ~a: [a_class ["d-none"; "d-sm-inline"]] [txt "Type"]]; th [txt "Name"]; th [txt "Kind"]; th [txt "By"]]
                              ]
                          )
                      else None
                    )
                    ?tfoot: (
                      if show_table_headers then
                        Some
                          (
                            tfoot
                              ~a: [a_class ["table-primary"]]
                              [
                                tr [th [span ~a: [a_class ["d-none"; "d-sm-inline"]] [txt "Type"]]; th [txt "Name"]; th [txt "Kind"]; th [txt "By"]]
                              ]
                          )
                      else None
                    )
                    [
                      R.tbody
                        (
                          flip S.map (S.Pair.pair (slice t.pagination) (SearchBar.state t.search_bar))
                            @@ fun (_, state) ->
                            match state with
                            | Results results ->
                              let context = S.map Common.Endpoints.Page.inSearch @@ SearchBar.text t.search_bar in
                              List.map Utils.(ResultRow.to_clickable_row % (make_result ~context)) results
                            | _ -> []
                        )
                    ];
                ];
              (
                match t.pagination with
                | Pagination p -> Pagination.render ~is_below: true p
                | FixedSlice _ -> div []
              )
            ]
        );
        (
          (* A placeholder version of the result table. It only shows when
             neither the info/alert box nor the results table shows. *)
          div
            ~a: [
              R.a_class (
                flip S.map (SearchBar.state t.search_bar) @@ function
                  | Searching -> ["my-4"]
                  | _ -> ["d-none"]
              )
            ]
            (
              [
              match t.pagination with
              | Pagination _ -> Pagination.placeholder ~is_below: false ()
              | FixedSlice _ -> div []] @
              Tables.placeholder ~show_thead: show_table_headers ~show_tfoot: show_table_headers () @ [
                match t.pagination with
                | Pagination _ -> Pagination.placeholder ~is_below: true ()
                | FixedSlice _ -> div []
              ]
            )
        );
      ]
end
include Search

module Quick = struct
  type 'result t = {search: 'result Search.t}

  let text quick_search = SearchBar.text @@ Search.search_bar quick_search.search

  let make
    ~search
    ?on_enter
    ()
  = {
    search =
    Search.make
      ~search
      ~pagination_mode: (FixedSlice (Slice.make ~start: 0 ~end_excl: 20 ()))
      ~min_characters: 3
      ?on_enter
      ();
  }

  let render
      ~return
      ~dialog_title
      ?(dialog_buttons = [])
      ~make_result
      quick_search
    =
    Page.make'
      ~title: dialog_title
      ~on_load: (fun () -> SearchBar.focus @@ Search.search_bar quick_search.search)
      [Search.render
        ~make_result
        ~show_table_headers: false
        quick_search.search;
      ]
      ~buttons: (
        Button.cancel' ~return () :: dialog_buttons
      )
end
