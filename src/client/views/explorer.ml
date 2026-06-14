open Nes
open Dancelor_common
open Search_new
open Js_of_ocaml
open Components
open Html
open Utils

let update_uri input =
  let uri = Endpoints.Page.(href Explore) (Some input) in
  Dom_html.window##.history##replaceState
    "fixme-the-state"
    (Js.string "")
    (Js.some (Js.string (Uri.to_string uri)))

let view query =
  let search =
    Search.make
      ~search: (fun slice query ->
        match Any_query.parse query with
        | Error msg -> lwt_error msg
        | Ok query -> ok <$> Madge_client.call_exn Endpoints.Api.(route @@ Any Search) slice query
      )
      ?initial_input: query
      ~pagination_mode: (Pagination ())
      ~on_input: update_uri
      ()
  in
  Page.make'
    ~title: (lwt "Explore")
    ~on_load: (fun () -> Search_bar.focus @@ Search.search_bar search)
    [
      Search.render
        search
        ~make_result: (fun ?context result -> Any_result_new.make_result ?context result)
        ~attached_buttons: [
          Button.make
            ~label: "Filter"
            ~label_processing: "Filtering..."
            ~icon: (Other Filter)
            ~classes: ["btn-primary"]
            ~onclick: (fun () ->
              let search_text = S.value @@ Search_bar.text @@ Search.search_bar search in
              let%lwt query = Search_complex_filters_dialog.open_ search_text in
              Option.iter
                (fun query ->
                  let text = Any_query.print query ^ " " in
                  let bar = Search.search_bar search in
                  Search_bar.set_text bar text;
                  update_uri text;
                  Search_bar.focus bar
                )
                query;
              lwt_unit
            )
            ();
        ]
    ]
