open Nes
open Dancelor_client_html
open Dancelor_client_model

(** Generic row showing an emoji on the left and a message on the right. *)
let fa_row icon message =
  tr [
    td ~a:[a_colspan 0] [
      i ~a:[a_class ["material-symbols-outlined"]] [txt icon];
      txt " ";
      txt message;
    ];
  ]

(* FIXME: We should push the selection into the search bar, instead of
   providing a [make_result] that gives a clickable row. That way, we could
   improve the search bar to work with keys and not just clicks. *)

type 'result t = {
  min_characters : int;
  search_bar : 'result SearchBar.t;
}

let clear q = SearchBar.clear q.search_bar

let make ?(number_of_results=10) ~search () =
  let min_characters = 3 in
  let slice = S.const @@ Slice.make ~start:0 ~end_excl:number_of_results () in
  let search_bar =
    SearchBar.make
      ~search
      ~min_characters
      ~slice
      ()
  in
  {min_characters; search_bar}

let render ~placeholder ~make_result ?on_enter ?autofocus q =

  (** A signal tracking whether the table is focused. *)
  let (table_visible, set_table_visible) = S.create false in

  div ~a:[a_class ["search-bar"]] [
    div ~a:[
      R.a_class (
        Fun.flip S.map table_visible @@ function
        | false -> []
        | true -> ["visible"]
      );
      a_onclick (fun _ -> set_table_visible false; false);
    ] [];

    SearchBar.render ~placeholder ~on_focus:(fun () -> set_table_visible true) ?on_enter ?autofocus q.search_bar;

    tablex
      ~a:[
        R.a_class (
          Fun.flip S.map table_visible @@ function
          | false -> ["dropdown-table"]
          | true -> ["dropdown-table"; "visible"]
        );
      ]
      [
        R.tbody (
          S.bind_s' (SearchBar.state q.search_bar) [] @@ function
          | StartTyping -> Lwt.return [fa_row "keyboard" "Start typing to search."]
          | ContinueTyping -> Lwt.return [fa_row "keyboard" (spf "Type at least %s characters." (Int.to_english_string q.min_characters))]
          | NoResults -> Lwt.return [fa_row "warning" "Your search returned no results."]
          | Errors error -> Lwt.return [fa_row "error" error]
          | Results results ->
            let%lwt results = Lwt_list.map_p make_result results in
            Lwt.return @@
            if on_enter = None then
              results
            else
              results @ [fa_row "info" "Press enter for more results."]
        );
      ]
  ]

let make_and_render ?number_of_results ~placeholder ~search ~make_result ?on_enter ?autofocus () =
  render ~placeholder ~make_result ?on_enter ?autofocus (make ?number_of_results ~search ())
