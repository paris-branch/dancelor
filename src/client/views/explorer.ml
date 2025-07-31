open Nes
open Common

open Js_of_ocaml
open Components
open Html

let update_uri input =
  let uri = Endpoints.Page.(href Explore) (Some input) in
  Dom_html.window##.history##replaceState
    "fixme-the-state"
    (Js.string "")
    (Js.some (Js.string uri))

let create ?query () =
  let search =
    Search.make
      ~search: (fun slice input ->
        let%rlwt filter = lwt (Filter.Any.from_string input) in
        ok <$> Madge_client.call_exn Endpoints.Api.(route @@ Any Search) slice filter
      )
      ?initial_input: query
      ~pagination_mode: (Pagination ())
      ~on_input: update_uri
      ()
  in
  Page.make'
    ~title: (lwt "Explore")
    ~on_load: (fun () -> SearchBar.focus @@ Search.search_bar search)
    [
      Search.render
        search
        ~make_result: (fun ~context result -> Utils.AnyResult.make_result ~context result)
        ~attached_buttons: [
          Button.make
            ~label: "Filter"
            ~label_processing: "Filtering..."
            ~icon: "filter"
            ~classes: ["btn-primary"]
            ~onclick: (fun () ->
              let search_text = S.value @@ SearchBar.text @@ Search.search_bar search in
              let%lwt text = SearchComplexFiltersDialog.open_ search_text in
              Option.iter
                (fun text ->
                  let text = text ^ " " in
                  let bar = Search.search_bar search in
                  SearchBar.set_text bar text;
                  update_uri text;
                  SearchBar.focus bar
                )
                text;
              lwt_unit
            )
            ();
        ]
    ]
