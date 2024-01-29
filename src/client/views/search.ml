open Nes
open Js_of_ocaml
open Dancelor_client_model
open Dancelor_client_components
module Elements = Dancelor_client_elements
module Formatters = Dancelor_client_formatters
module Utils = Dancelor_client_utils
module PageRouter = Dancelor_common_pageRouter

let js = Js.string

type t =
  {
    page : Elements.Page.t;
    content : Dom_html.divElement Js.t;
  }

let update_uri input =
  let query = [("q", [Yojson.Safe.to_string (`String input)])] in
  let uri = Uri.make ~path:"/search" ~query () in
  Dom_html.window##.history##replaceState
    "fixme-the-state" (js "") (Js.some (js (Uri.to_string uri)))

let search ?pagination input =
  let threshold = 0.4 in
  let%rlwt filter = Lwt.return (Any.Filter.from_string input) in
  Lwt.map Result.ok @@ Any.search ~threshold ?pagination filter

let preprocess additional_filter input =
  match Any.Filter.from_string input with
  | Error m -> React.S.const (Error m)
  | Ok filter -> React.S.map (Result.ok % Formula.and_ filter) additional_filter

let search' ?pagination filter =
  let threshold = 0.4 in
  Lwt.map Result.ok @@ Any.search ~threshold ?pagination filter

(** Generic row showing an emoji on the left and a message on the right. *)
let emoji_row emoji message =
  let open Dancelor_client_html in
  tr [
    td [txt emoji];
    td ~a:[a_colspan 4] [txt message];
  ]

let create ?query page =
  let document = Elements.Page.document page in
  let content = Dom_html.createDiv document in

  document##.title := js ("Search | Dancelor");

  let open Dancelor_client_html in

  let (number_of_entries, set_number_of_entries) = S.create None in

  let pagination =
    PageNav.create
      ~entries_per_page: 25
      ~number_of_entries
  in

  let type_choices =
    Choices.(make_checkboxes [
        choice [txt "Book"] ~value:Any.Type.Book;
        choice [txt "Dance"] ~value:Any.Type.Dance;
        choice [txt "Person"] ~value:Any.Type.Person;
        choice [txt "Set"] ~value:Any.Type.Set;
        choice [txt "Tune"] ~value:Any.Type.Tune;
        choice [txt "Version"] ~value:Any.Type.Version;
      ])
  in

  let additional_filter =
    Fun.flip S.map (Choices.signal type_choices) @@ function
    | [] -> Formula.true_
    | type_choices -> Formula.or_l (List.map Any.Filter.type_ type_choices)
  in

  let search_bar =
    SearchBar.make_pps
      ~preprocess:(preprocess additional_filter)
      ~search:(fun pagination input -> search' ~pagination input)
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

      div [
        Choices.render type_choices;
      ];

      R.div (
        Fun.flip S.map (SearchBar.state search_bar) @@ function
        | NoResults -> [div ~a:[a_class ["warning"]] [txt "Your search returned no results."]]
        | Errors error -> [div ~a:[a_class ["error"]] [txt error]]
        | StartTyping | ContinueTyping | Results _ -> []
      );

      div
        ~a:[
          R.a_class (
            Fun.flip S.map (SearchBar.state search_bar) @@ function
            | Results _ -> []
            | _ -> ["hidden"]
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
