open Nes
open Js_of_ocaml_tyxml.Tyxml_js
open Dancelor_client_html
module Model = Dancelor_client_model

(* TODO: Filter out from search an element that has already been selected. *)
(* TODO: Also store the search text in the raw signal. *)

type 'model t = {
  signal : 'model option S.t;
  set : 'model option -> unit;
  search_bar : 'model QuickSearchBar.t;
  serialise : 'model -> 'model Slug.t;
}

let make ~search ~serialise ~unserialise initial_value =
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
  {signal; set; search_bar; serialise}

let raw_signal s = S.map (Option.map s.serialise) s.signal

let signal (s : 'model t) : ('model option, string) Result.t S.t =
  S.map Result.ok s.signal

let signal_non_empty (s : 'model t) : ('model, string) Result.t S.t =
  S.map (Option.to_result ~none:"Must select something") s.signal

let clear s =
  s.set None;
  QuickSearchBar.clear s.search_bar

let render
    ~(make_result:
        ?classes: string list ->
      ?onclick: (unit -> unit) ->
      ?prefix: Html_types.td Html.elt list ->
      ?suffix: Html_types.td Html.elt list ->
      'result ->
      Html_types.tr Html.elt
     )
    ~field_name
    ~model_name
    ~(create_dialog_content:
        ?on_save:('result -> unit) ->
      unit ->
      Html_types.div Html.elt
     )
    s
  =
  div ~a:[a_class ["form-element"]] [
    label [txt (fst field_name)];

    tablex ~a:[a_class ["container"]] [
      R.tbody (
        Fun.flip S.map s.signal @@ fun maybe_element ->
        List.map
          (fun element ->
             make_result
               ~classes: ["row"]
               ~onclick: (fun () -> ())
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
      ~make_result: (fun person ->
          Lwt.return @@ make_result
            ~onclick:(fun () ->
                s.set (Some person);
                QuickSearchBar.clear s.search_bar;
              )
            ~suffix:[]
            person
        )
      ~more_lines: [
        QuickSearchBar.fa_row
          ~onclick:(fun () ->
              Lwt.async @@ fun () ->
              let%lwt result = Dialog.open_ @@ fun return ->
                QuickSearchBar.clear s.search_bar;
                [create_dialog_content ~on_save:return ()]
              in
              Result.iter (fun element ->
                  s.set (Some element);
                ) result;
              Lwt.return_unit
            )
          "add_circle" ("Create a new " ^ model_name)
      ]
      s.search_bar;
    div ~a:[a_class ["message-box"]] [];
  ]
