open Nes
open Html
open Utils

module Search = struct
  type 'p pagination_mode =
    | Pagination of 'p
    | Fixed_slice of Slice.t

  let slice = function
    | Pagination p -> (Pagination.slice p, (fun () -> Pagination.reset p))
    | Fixed_slice s -> (S.const s, Fun.const ())

  type 'result t = {
    pagination: Pagination.t pagination_mode;
    search_bar: 'result Search_bar.t;
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
      | Fixed_slice slice -> Fixed_slice slice
    in
    let search_bar =
      Search_bar.make
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
      ~(make_result : ?context: Common.Endpoints.Page.context S.t -> 'result -> Html_types.tr Html.elt)
      ?(results_when_no_search = [])
      ?(attached_buttons = [])
      ?(show_table_headers = true)
      t
    =
    div
      [
        (
          (* Alert for when the user is not connected. *)
          R.div
            ~a: [a_class ["mb-3"]]
            (
              S.from' [] @@
                match%lwt Madge_client.call_exn Common.Endpoints.Api.(route @@ User Status) with
                | Some _ -> lwt_nil
                | None -> lwt [Alert.make ~level: Info [txt "You are not connected, and therefore are only seeing public items."]]
            )
        );
        (
          (* The search bar. *)
          div
            ~a: [a_class ["input-group"; "mb-3"]]
            (
              [Icon.html ~classes: ["input-group-text"] (Action Search);
              Search_bar.html t.search_bar;
              ] @
                attached_buttons
            )
        );
        (
          (* Info or alert box, eg. to inform that one needs to type. *)
          R.div
            (
              let no_search_suffix = if results_when_no_search <> [] then " Maybe you want one of the following:" else "" in
              S.flip_map (Search_bar.state t.search_bar) @@ function
                | No_results -> [Alert.make ~level: Warning [txtf "Your search returned no results.%s" no_search_suffix]]
                | Errors error -> [Alert.make ~level: Danger [txtf "%s%s" error no_search_suffix]]
                | Start_typing -> [Alert.make ~level: Info [txtf "Start typing to search.%s" no_search_suffix]]
                | Continue_typing -> [Alert.make ~level: Info [txtf "Type at least %s characters.%s" (Int.to_english_string t.min_characters) no_search_suffix]]
                | Searching | Results _ -> []
            )
        );
        (
          (* The results table *)
          div
            ~a: [
              R.a_class
                (
                  S.flip_map (Search_bar.state t.search_bar) @@ fun state ->
                  let show =
                    match state with
                    | Results _ -> true
                    | Start_typing | Continue_typing | No_results | Errors _ -> results_when_no_search <> []
                    | _ -> false
                  in
                  if show then if show_table_headers then ["my-4"] else [] else ["d-none"]
                )
            ]
            [
              (
                match t.pagination with
                | Pagination p -> Pagination.render ~is_below: false p
                | Fixed_slice _ -> div []
              );
              div
                ~a: [a_class ["table-responsive"]]
                [
                  let make_table_headers (thead : ('a, 'b, [< Html_types.thead | Html_types.tfoot]) Html.star) =
                    thead
                      ~a: [a_class ["table-primary"]]
                      [
                        tr [
                          th [span ~a: [a_class ["d-none"; "d-sm-inline"]] [txt "Type"]];
                          th [txt "Name"];
                          th [txt "Kind/date"];
                          th [txt "By"];
                          th []
                        ]
                      ]
                  in
                  tablex
                    ~a: [a_class ["table"; "table-striped"; "table-hover"; "table-borderless"; "my-1"]]
                    ?thead: (if show_table_headers then Some (make_table_headers thead) else None)
                    ?tfoot: (if show_table_headers then Some (make_table_headers tfoot) else None)
                    [
                      R.tbody
                        (
                          S.flip_map (S.Pair.pair (fst @@ slice t.pagination) (Search_bar.state t.search_bar))
                            @@ fun (_, state) ->
                            match state with
                            | Results results ->
                              let context = S.map Common.Endpoints.Page.in_search @@ Search_bar.text t.search_bar in
                              List.map (make_result ~context) results
                            | Start_typing | Continue_typing | No_results | Errors _ ->
                              List.map make_result results_when_no_search
                            | Searching -> []
                        )
                    ];
                ];
              (
                match t.pagination with
                | Pagination p -> Pagination.render ~is_below: true p
                | Fixed_slice _ -> div []
              )
            ]
        );
        (
          (* A placeholder version of the result table. It only shows when
             neither the info/alert box nor the results table shows. *)
          div
            ~a: [
              R.a_class (
                S.flip_map (Search_bar.state t.search_bar) @@ function
                  | Searching -> ["my-4"]
                  | _ -> ["d-none"]
              )
            ]
            (
              [
              match t.pagination with
              | Pagination _ -> Pagination.placeholder ~is_below: false ()
              | Fixed_slice _ -> div []] @
              Tables.placeholder ~show_thead: show_table_headers ~show_tfoot: show_table_headers () @ [
                match t.pagination with
                | Pagination _ -> Pagination.placeholder ~is_below: true ()
                | Fixed_slice _ -> div []
              ]
            )
        );
      ]
end
include Search

module Quick = struct
  type 'result t = {search: 'result Search.t}

  let search_bar quick_search = search_bar quick_search.search

  let text quick_search = Search_bar.text @@ Search.search_bar quick_search.search

  let make
    ~search
    ?on_enter
    ()
  = {
    search =
    Search.make
      ~search
      ~pagination_mode: (Fixed_slice (Slice.make ~start: 0 ~end_excl: 20 ()))
      ~min_characters: 3
      ?on_enter
      ();
  }

  let render
      ~return
      ~dialog_title
      ?(dialog_buttons = [])
      ~make_result
      ?results_when_no_search
      quick_search
    =
    Page.make'
      ~title: dialog_title
      ~on_load: (fun () -> Search_bar.focus @@ Search.search_bar quick_search.search)
      [Search.render
        ~make_result
        ?results_when_no_search
        ~show_table_headers: false
        quick_search.search;
      ]
      ~buttons: (Button.cancel' ~return () :: dialog_buttons)
end
