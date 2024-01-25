open Nes
open Js_of_ocaml
open Dancelor_client_model
open Dancelor_client_components
module Elements = Dancelor_client_elements
module Formatters = Dancelor_client_formatters
module Utils = Dancelor_client_utils
module PageRouter = Dancelor_common_pageRouter

module Html = Dom_html

let js = Js.string

type t =
  {
    page : Elements.Page.t;
    content : Html.divElement Js.t;
  }

let update_uri input =
  Dom_html.window##.history##replaceState
    "fixme-the-state" (js "") (Js.some (js (spf "/search?q=%s" (Yojson.Safe.to_string (`String input)))))

let search pagination input =
  let threshold = 0.4 in
  let%rlwt filter = Lwt.return (Any.Filter.from_string input) in
  let%lwt (total, results) = Any.search ~threshold ~pagination filter in
  Format.printf "Got a search with %d results out of %d total.@." (List.length results) total;
  Lwt.return_ok (total, results)

(** Generic row showing an emoji on the left and a message on the right. *)
let emoji_row emoji message =
  let open Dancelor_client_html in
  tr [
    td [txt emoji];
    td ~a:[a_colspan 4] [txt message];
  ]

let create ?query page =
  let document = Elements.Page.document page in
  let content = Html.createDiv document in

  document##.title := js ("Search | Dancelor");

  let open Dancelor_client_html in

  let (number_of_entries, set_number_of_entries) = S.create None in

  let pagination =
    PageNav.create
      ~entries_per_page: 25
      ~number_of_entries
  in

  let search_bar =
    SearchBar.make
      ~search
      ~pagination:(PageNav.pagination pagination)
      ~on_number_of_entries: (set_number_of_entries % Option.some)
      ?initial_input: query
      ()
  in

  (
    Dom.appendChild content @@ To_dom.of_div @@ div [
      h2 ~a:[a_class ["title"]] [txt "Search"];

      SearchBar.render
        ~placeholder:"Search for anything (it really is magic!)"
        ~autofocus:true
        ~on_input:update_uri
        search_bar;

      R.div (
        Fun.flip S.map (SearchBar.state search_bar) @@ function
        | NoResults -> [div ~a:[a_class ["warning"]] [txt "Your search returned no results."]]
        | Errors errors -> [div ~a:[a_class ["error"]] [ul (List.map (li % List.singleton % txt) errors)]]
        | StartTyping | ContinueTyping | Results _ -> []
      );

      div
        ~a:[
          R.a_class (
            Fun.flip S.map (SearchBar.state search_bar) @@ function
            | Results _ ->
              Format.printf "No class when resuts.@.";
              []
            | _ ->
              Format.printf "Should be hidden when no results.@.";
              ["hidden"]
          )
        ]
        [
          PageNav.render pagination;

          tablex
            ~a:[a_class ["separated-table"]]
            ~thead:(
              thead [
                tr [th [txt "Score"]; th [txt "Type"]; th [txt "Name"]; th [txt "Kind"]; th [txt "By"];]
              ];
            )
            [
              R.tbody (
                Fun.flip S.map (S.Pair.pair (PageNav.pagination pagination) (SearchBar.state search_bar))
                @@ fun (_, state) ->
                match state with
                | Results results ->
                  let context = Option.map PageRouter.inSearch query in
                  List.map Utils.AnyResultNewAPI.(make_result ?context) results
                | _ -> []
              )
            ];

          PageNav.render pagination;
        ]
    ]
  );

  { page; content }

let contents t =
  t.content

let refresh t =
  ignore t

let init t =
  ignore t
