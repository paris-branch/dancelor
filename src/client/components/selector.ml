open Js_of_ocaml
open Nes
open Common
open Html

(* TODO: Also store the search text in the raw signal. *)

let prepare (type model)
  ~label
  ~search
  ~serialise
  ~unserialise
  ~(make_result :
    ?classes: string list ->
    ?action: Utils.ResultRow.action ->
    ?prefix: Utils.ResultRow.cell list ->
    ?suffix: Utils.ResultRow.cell list ->
    model Entry.t ->
    Utils.ResultRow.t
  )
  ?(make_more_results =
  (const []: model Entry.t ->
    Utils.ResultRow.t list))
  ~model_name
  ~(create_dialog_content :
    ?on_save: (model Entry.t -> unit) ->
    string ->
    Page.t Lwt.t
  )
  ()
=
((module struct
  let label = label

  type value = model Entry.t
  type raw_value = model Entry.Id.t option

  let empty_value = None

  type t = {
    signal: model Entry.t option S.t;
    set: model Entry.t option -> unit;
    (* quick_search: model Entry.t Search.Quick.t; *)
    serialise: model Entry.t -> model Entry.Id.t;
    inner_html: Html_types.div_content_fun elt;
    select_button_dom: Dom_html.buttonElement Js.t;
  }

  let raw_signal s = S.map (Option.map s.serialise) s.signal
  let signal i = S.map (Option.to_result ~none: "You must select an element.") i.signal
  let inner_html s = s.inner_html

  let focus s = s.select_button_dom##focus
  let trigger s = s.select_button_dom##click

  let clear s = s.set None

  let make initial_value =
    let (signal, set) = S.create None in
    let quick_search = Search.Quick.make ~search () in
    Lwt.async (fun () ->
      let%lwt initial_value =
        match initial_value with
        | None -> lwt_none
        | Some initial_value -> some <$> unserialise initial_value
      in
      set initial_value;
      lwt_unit
    );
    let select_model () =
      let%lwt quick_search_result =
        Page.open_dialog @@ fun quick_search_return ->
        Search.Quick.render
          quick_search
          ~return: quick_search_return
          ~dialog_title: (lwt label)
          ~make_result: (fun ~context: _ result ->
            make_result
              ~action: (Utils.ResultRow.callback @@ fun () -> quick_search_return (Some result))
              result
          )
          ~dialog_buttons: [
            Button.make
              ~label: ("Create new " ^ model_name)
              ~label_processing: ("Creating new " ^ model_name ^ "...")
              ~icon: "plus-circle"
              ~classes: ["btn-primary"]
              ~onclick: (fun () ->
                quick_search_return
                <$> Page.open_dialog' @@ fun sub_dialog_return ->
                  create_dialog_content
                    ~on_save: sub_dialog_return
                    (S.value (Search.Quick.text quick_search))
              )
              ();
          ]
      in
      SearchBar.clear @@ Search.Quick.search_bar quick_search;
      flip Option.iter quick_search_result (fun r -> set (Some r));
      lwt_unit
    in
    let select_button =
      Button.make
        ~label: ("Select a " ^ model_name)
        ~label_processing: ("Selecting a " ^ model_name ^ "...")
        ~classes: ["btn-outline-secondary"; "w-100"; "text-start"]
        ~onclick: select_model
        ()
    in
    let select_button_dom = To_dom.of_button select_button in
    let inner_html =
      R.div (
        S.map List.singleton @@
        flip S.map signal @@ function
        | None ->
          (
            div
              ~a: [a_class ["btn-group"; "w-100"]]
              [
                Button.make_icon "search" ~classes: ["btn-light"];
                select_button;
              ];
          )
        | Some model ->
          tablex
            ~a: [a_class ["table"; "table-borderless"; "table-sm"; "m-0"]]
            [
              tbody (
                List.map
                  Utils.ResultRow.to_clickable_row
                  (make_result model :: make_more_results model)
              )
            ]
      )
    in
      {signal; set; serialise; inner_html; select_button_dom}
end):
  (model Entry.t, model Entry.Id.t option) Component.s)

let make
    ~label
    ~search
    ~serialise
    ~unserialise
    ~make_result
    ?make_more_results
    ~model_name
    ~create_dialog_content
    initial_value
  =
  Component.initialise
    (
      prepare
        ~label
        ~search
        ~serialise
        ~unserialise
        ~make_result
        ?make_more_results
        ~model_name
        ~create_dialog_content
        ()
    )
    initial_value
