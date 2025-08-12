open Js_of_ocaml
open Nes
open Common
open Html

(* TODO: Also store the search text in the raw signal. *)

let prepare (type model)
  ~label
  ~search
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
  : (model Entry.t, model Entry.Id.t option) Component.s
= (module struct
  let label = label

  type value = model Entry.t

  (* Dirty trick to convince Yojson to serialise ids. *)
  let model_to_yojson _ = assert false
  let model_of_yojson _ = assert false

  type raw_value = model Entry.Id.t option [@@deriving yojson]

  let empty_value = None
  let raw_value_from_initial_text _ = None
  let serialise = some % Entry.id

  type t = {
    signal: model Entry.t option S.t;
    set: model Entry.t option -> unit;
    (* quick_search: model Entry.t Search.Quick.t; *)
    inner_html: Html_types.div_content_fun elt;
    select_button_dom: Dom_html.buttonElement Js.t;
  }

  let raw_signal s = S.map (flip Option.bind serialise) s.signal
  let signal i = S.map (Option.to_result ~none: "You must select an element.") i.signal
  let inner_html s = s.inner_html

  let actions s =
    flip S.map s.signal @@ function
      | None -> []
      | Some _ ->
        [
          Button.make
            ~classes: ["btn-warning"]
            ~icon: "eraser"
            ~onclick: (fun _ -> s.set None; lwt_unit)
            ();
          Button.make
            ~classes: ["btn-info"]
            ~icon: "pencil-square"
            ~onclick: (fun _ -> s.select_button_dom##click; lwt_unit)
            ();
        ]

  let focus s = s.select_button_dom##focus
  let trigger s = s.select_button_dom##click

  let set _ _ = assert false

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
        flip S.map signal @@ function
          | None ->
            [
              div
                ~a: [a_class ["btn-group"; "w-100"]]
                [
                  Button.make_icon "search" ~classes: ["btn-light"];
                  select_button;
                ];
            ]
          | Some model ->
            [
              div ~a: [a_class ["row"; "m-0"]] [
                tablex
                  ~a: [a_class ["table"; "table-borderless"; "table-sm"; "m-0"; "col"]]
                  [tbody (List.map Utils.ResultRow.to_clickable_row [make_result model])];
              ];
              div ~a: [a_class ["row"; "m-0"; "overflow-hidden"]] [
                tablex
                  ~a: [a_class ["table"; "table-borderless"; "table-sm"; "m-0"; "col"]]
                  [tbody (List.map Utils.ResultRow.to_clickable_row (make_more_results model))];
              ]
            ]
      )
    in
      {signal; set; inner_html; select_button_dom}
end)

let make
    ~label
    ~search
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
        ~unserialise
        ~make_result
        ?make_more_results
        ~model_name
        ~create_dialog_content
        ()
    )
    initial_value
