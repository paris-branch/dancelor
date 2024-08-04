open Nes
open Js_of_ocaml_tyxml.Tyxml_js
open Dancelor_client_html
module Model = Dancelor_client_model
module Utils = Dancelor_client_utils

(* TODO: Filter out from search an element that has already been selected. *)
(* TODO: Also store the search text in the raw signal. *)

type 'model t = {
  has_interacted : bool S.t;
  set_interacted : unit -> unit;
  signal : 'model option S.t;
  set : 'model option -> unit;
  search_bar : 'model QuickSearchBar.t;
  serialise : 'model -> 'model Slug.t;
}

let make ?(has_interacted=S.const false) ~search ~serialise ~unserialise initial_value =
  let (has_interacted_locally, set_interacted) = S.create false in
  let has_interacted = S.l2 (||) has_interacted has_interacted_locally in
  let set_interacted () =
    set_interacted true;
    S.stop ~strong:true has_interacted
  in
  let (signal, set) = S.create None in
  let search_bar = QuickSearchBar.make
      ~number_of_results: 5
      ~search
      ()
  in
  Lwt.async (fun () ->
      let%lwt initial_value = Option.fold ~none:Lwt.return_none ~some:(Lwt.map Option.some % unserialise) initial_value in
      set initial_value;
      Lwt.return_unit
    );
  {has_interacted; set_interacted; signal; set; search_bar; serialise}

let raw_signal s = S.map (Option.map s.serialise) s.signal

let signal (s : 'model t) : ('model, string) Result.t S.t =
  S.map (Option.to_result ~none:"Must select something") s.signal

let has_interacted state = state.has_interacted

let case_errored ~no ~yes state =
  S.bind (has_interacted state) @@ fun has_interacted ->
  Fun.flip S.map (signal state) @@ function
  | Error msg when has_interacted -> yes msg
  | _ -> no

let clear s =
  s.set None;
  QuickSearchBar.clear s.search_bar

let render
    ~(make_result:
        ?classes: string list ->
      ?action: Utils.ResultRow.action ->
      ?prefix: Html_types.td Html.elt list ->
      ?suffix: Html_types.td Html.elt list ->
      'result ->
      Utils.ResultRow.t
     )
    ~field_name
    ~model_name
    ~(create_dialog_content:
        ?on_save:('result -> unit) ->
      string ->
      Html_types.div Html.elt
     )
    s
  =
  div
    ~a:[
      R.a_class (case_errored ~no:["form-element"] ~yes:(Fun.const ["form-element"; "invalid"]) s);
    ]
    [
      label [txt (fst field_name)];

      tablex ~a:[a_class ["container"]] [
        R.tbody (
          Fun.flip S.map s.signal @@ fun maybe_element ->
          List.map Utils.ResultRow.to_clickable_row @@
          List.map
            (fun element ->
               make_result
                 ~classes: ["row"]
                 ~action: Utils.ResultRow.noAction
                 ~suffix: [
                   td ~a:[a_class ["actions"]] [
                     button
                       ~a: [
                         a_onclick (fun _ -> s.set None; true);
                         a_class ["btn-danger"];
                       ]
                       [i ~a:[a_class ["material-symbols-outlined"]] [txt "delete"]];
                   ]
                 ]
                 element
            )
            (Option.to_list maybe_element)
        )
      ];

      QuickSearchBar.render
        ~placeholder: ("Select a " ^ snd field_name ^ " (magic search)")
        ~on_focus: s.set_interacted
        ~make_result: (fun person ->
            make_result
              ~action: (Utils.ResultRow.callback @@ fun () ->
                        s.set (Some person);
                        QuickSearchBar.clear s.search_bar;
                       )
              ~suffix:[]
              person
          )
        ~more_lines: [
          Utils.ResultRow.icon_row
            ~action: (Utils.ResultRow.callback @@ fun () ->
                      Lwt.async @@ fun () ->
                      let%lwt result = Dialog.open_ @@ fun return ->
                        QuickSearchBar.clear s.search_bar;
                        [
                          create_dialog_content
                            ~on_save:return
                            (S.value (SearchBar.text (QuickSearchBar.search_bar s.search_bar)))
                        ]
                      in
                      Result.iter (fun element ->
                          s.set (Some element);
                        ) result;
                      Lwt.return_unit
                     )
            "add_circle" ("Create a new " ^ model_name)
        ]
        s.search_bar;

      R.div ~a:[a_class ["message-box"]] (
        case_errored ~no:[] ~yes:(List.singleton % txt) s
      );
    ]
