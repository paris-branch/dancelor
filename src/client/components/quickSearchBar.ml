open Nes
open Js_of_ocaml
open Dancelor_client_html
open Dancelor_client_model
module Utils = Dancelor_client_utils

let rec is_child_of : 'a 'b. ((#Dom.node as 'a) Js.t) -> ((#Dom.node as 'b) Js.t) -> bool =
  fun c p ->
  ((c :> Dom.node Js.t) = (p :> Dom.node Js.t)) ||
  (Js.Opt.case c##.parentNode
     (Fun.const false)
     (fun p' -> is_child_of p' p))

let is_input : 'a. (#Dom.element as 'a) Js.t -> bool =
  fun n ->
  let tag = String.lowercase_ascii (Js.to_string n##.tagName) in
  tag = "input" || tag = "textarea"

let add_target_event_listener n ev f =
  let open Dom_html in
  ignore @@ addEventListener n ev
    (handler @@ fun event ->
     Js.Opt.case event##.target (fun () -> Js._false) (fun target -> f event target)
    ) Js._true

(** Generic row showing an emoji on the left and a message on the right. *)
let fa_row ?onclick icon message =
  Utils.ResultRow.make ?action:onclick
    [
      td ~a:[a_colspan 9999] [

        i ~a:[a_class ["material-symbols-outlined"]] [txt icon];
        txt " ";
        txt message;
      ];
    ]

type 'result t = {
  min_characters : int;
  search_bar : 'result SearchBar.t;
  table_visible : bool S.t;
  set_table_visible : bool -> unit;
}

let search_bar q = q.search_bar

let clear q =
  SearchBar.clear q.search_bar;
  q.set_table_visible false

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
  (** A signal tracking whether the table is focused. *)
  let (table_visible, set_table_visible) = S.create false in
  {min_characters; search_bar; table_visible; set_table_visible}

let render ~placeholder ~make_result ?on_enter ?on_focus ?(more_lines=[]) ?autofocus ?(focus_on_slash=false) q =
  let bar =
    SearchBar.render
      ~placeholder
      ~on_focus: (fun () ->
          Option.iter (fun on_focus -> on_focus ()) on_focus;
          q.set_table_visible true
        )
      ?on_enter
      ?autofocus
      q.search_bar
  in
  let table =
    tablex
      ~a:[
        R.a_class (
          Fun.flip S.map q.table_visible @@ function
          | false -> ["dropdown-table"]
          | true -> ["dropdown-table"; "visible"]
        );
      ]
      [
        R.tbody (
          S.bind_s' (SearchBar.state q.search_bar) [] @@ fun result ->
          let%lwt lines =
            match result with
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
          in Lwt.return (List.map Utils.ResultRow.to_clickable_row (lines @ more_lines))
        );
      ]
  in
  (* Add an event listener to hide the table by clicking outside of it. *)
  add_target_event_listener Dom_html.window Dom_html.Event.click (fun _event target ->
      if not (is_child_of target (To_dom.of_table table)) && not (is_child_of target (To_dom.of_input bar)) then
        q.set_table_visible false;
      Js._true
    );
  (* Add an event listener to unfocus the bar on Esc. *)
  add_target_event_listener (To_dom.of_input bar) Dom_html.Event.keydown
    (fun event _target ->
       if event##.keyCode = 27 then (* Esc *)
         (
           q.set_table_visible false;
           (To_dom.of_input bar)##blur;
         );
       Js._true
    );
  (* Add an event listener to hide the table on Tab. *)
  add_target_event_listener (To_dom.of_input bar) Dom_html.Event.keydown
    (fun event _target ->
       if event##.keyCode = 9 then (* Tab *)
         q.set_table_visible false;
       Js._true
    );
  (* Add an event listener to focus the bar by pressing '/'. *)
  if focus_on_slash then
    add_target_event_listener Dom_html.window Dom_html.Event.keydown
      (fun event target ->
         if not (is_input target) && event##.keyCode = 191 then (* slash *)
           (
             (To_dom.of_input bar)##focus;
             Js._false
           )
         else
           Js._true
      );
  (* Return *)
  div ~a:[a_class ["quick-search-bar"]] [bar; table]

let make_and_render ?number_of_results ~placeholder ~search ~make_result ?on_enter ?more_lines ?autofocus ?focus_on_slash () =
  render ~placeholder ~make_result ?on_enter ?more_lines ?autofocus ?focus_on_slash (make ?number_of_results ~search ())
