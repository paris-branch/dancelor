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
        let%rlwt filter = Lwt.return (Model.Any.Filter.from_string input) in
        Lwt.map Result.ok @@
          Madge_client.call Endpoints.Api.(route @@ Any Search) slice filter
      )
      ?initial_input: query
      ~pagination_mode: (Pagination ())
      ()
  in
  Page.make
    ~title: (S.const "Explore")
    [
      L.div
        (
          match%lwt Madge_client.call Endpoints.Api.(route @@ Auth Status) with
          | Some _ -> Lwt.return_nil
          | None ->
            Lwt.return [
              div ~a: [a_class ["alert"; "alert-info"]; a_role ["alert"]] [
                txt "You are not connected, and therefore are only seeing public items.";
              ];
            ]
        );
      Search.render
        search
        ~make_result: (fun ~context result -> Utils.AnyResult.make_result ~context result)
        ~on_input: update_uri
        ~attached_buttons: [
          Button.make
            ~label: "Filter"
            ~label_processing: "Filtering..."
            ~icon: "filter"
            ~classes: ["btn-primary"]
            ~onclick: (fun () ->
              let search_text = S.value @@ SearchBar.text @@ Search.search_bar search in
              (* TODO: On return, add a space and focus the search bar. *)
              Lwt.map
                (Option.iter (fun text -> SearchBar.set_text (Search.search_bar search) text; update_uri text))
                (SearchComplexFiltersDialog.open_ search_text)
            )
            ();
        ]
    ]
