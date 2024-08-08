open Nes
open Js_of_ocaml
open Dancelor_client_html
open Dancelor_client_model
module Utils = Dancelor_client_utils

type 'result t = {
  min_characters : int;
  search_bar : 'result SearchBar.t;
  table_visible : bool S.t;
  set_table_visible : bool -> unit;
  selected_row : int option S.t;
  set_selected_row : int option -> unit;
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
  (** A signal tracking which row has been selected. *)
  let (selected_row, set_selected_row) = S.create None in
  {min_characters; search_bar; table_visible; set_table_visible; selected_row; set_selected_row}

let render
    ~placeholder
    ~(make_result: ?classes:string list -> 'result -> Utils.ResultRow.t)
    ?on_enter
    ?on_focus
    ?(more_lines=[])
    ?autofocus
    ?(focus_on_slash=false)
    (q: 'result t)
  =
  let bar =
    SearchBar.render
      ~placeholder
      ~on_focus: (fun () ->
          Option.iter (fun on_focus -> on_focus ()) on_focus;
          q.set_table_visible true
        )
      ~on_enter: (fun input ->
          match S.value (SearchBar.state q.search_bar) with
          | Results results ->
            (
              let number_of_lines = (if on_enter = None then 0 else 1) + List.length results in
              match S.value q.selected_row with
              | Some selected_row ->
                (
                  match List.nth_opt results ((selected_row + number_of_lines) mod number_of_lines) with
                  | Some result -> Utils.ResultRow.run_action @@ make_result result
                  | None -> Option.value on_enter ~default:ignore input
                )
              | None -> Option.value on_enter ~default:ignore input
            )
          | _ -> Option.value on_enter ~default:ignore input
        )
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
          S.bind q.selected_row @@ fun selected_row ->
          Fun.flip S.map (SearchBar.state q.search_bar) @@ fun result ->
          let lines =
            match result with
            | StartTyping -> [Utils.ResultRow.icon_row "keyboard" "Start typing to search."]
            | ContinueTyping -> [Utils.ResultRow.icon_row "keyboard" (spf "Type at least %s characters." (Int.to_english_string q.min_characters))]
            | NoResults -> [Utils.ResultRow.icon_row "warning" "Your search returned no results."]
            | Errors error -> [Utils.ResultRow.icon_row "error" error]
            | Results results ->
              let number_of_lines = (if on_enter = None then 0 else 1) + List.length results in
              let results = List.mapi (fun i result ->
                  let classes = match selected_row with
                    | Some selected_row when (selected_row + number_of_lines) mod number_of_lines = i -> Some ["selected"]
                    | _ -> None
                  in
                  make_result ?classes result)
                  results
              in
              if on_enter = None then
                results
              else
                results @ [
                  match selected_row with
                  | Some selected_row when (selected_row + number_of_lines) mod number_of_lines = List.length results ->
                    Utils.ResultRow.icon_row ~classes:["selected"] "info" "Press enter for more results."
                  | None -> Utils.ResultRow.icon_row "info" "Press enter for more results."
                  | _ -> Utils.ResultRow.icon_row "info" "Press enter to visit result."
                ]
          in List.map Utils.ResultRow.to_clickable_row (lines @ more_lines)
        );
      ]
  in
  let bar' = To_dom.of_input bar in
  let table' = To_dom.of_table table in

  (* Add an event listener to focus the bar by pressing '/'. *)
  if focus_on_slash then
    Utils.add_target_event_listener Dom_html.window Dom_html.Event.keydown
      (fun event target ->
         if not (Utils.is_input target) && event##.keyCode = 191 then (* slash *)
           (
             bar'##focus;
             Js._false
           )
         else
           Js._true
      );

  (* Add an event listener to hide the table by clicking outside of it. *)
  Utils.add_target_event_listener Dom_html.window Dom_html.Event.click (fun _event target ->
      if not (Utils.is_child_of target table') && not (Utils.is_child_of target bar') then
        q.set_table_visible false;
      Js._true
    );

  (* Add an event listener to hide the table on Tab or Esc, and to change the
     selected row by pressing KeyUp or KeyDown. *)
  Utils.add_target_event_listener bar' Dom_html.Event.keydown
    (fun event _target ->
       match event##.keyCode with
       | 9 (* Tab *) | 27 (* Esc *) ->
         (
           q.set_table_visible false;
           Js._true
         )
       | 38 (* KeyUp *) ->
         (
           q.set_selected_row (
             match S.value q.selected_row with
             | None -> Some (-1)
             | Some i -> Some (i - 1)
           );
           Js._false
         )
       | 40 (* KeyDown *) ->
         (
           q.set_selected_row (
             match S.value q.selected_row with
             | None -> Some 0
             | Some i -> Some (i + 1)
           );
           Js._false
         )
       | _ -> Js._true
    );

  (* Return *)
  div ~a:[a_class ["quick-search-bar"]] [bar; table]

let make_and_render ?number_of_results ~placeholder ~search ~make_result ?on_enter ?more_lines ?autofocus ?focus_on_slash () =
  render ~placeholder ~make_result ?on_enter ?more_lines ?autofocus ?focus_on_slash (make ?number_of_results ~search ())
