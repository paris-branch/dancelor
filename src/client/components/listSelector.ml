open Nes
open Js_of_ocaml_tyxml.Tyxml_js
open Dancelor_client_html
module Model = Dancelor_client_model

(* TODO: Filter out from search an element that has already been selected. *)
(* TODO: Also store the search text in the raw signal. *)

type 'model t = {
  signal : 'model list S.t;
  set : 'model list -> unit;
  search_bar : 'model QuickSearchBar.t;
  serialise : 'model -> 'model Slug.t;
}

let make ~search ~serialise ~unserialise initial_value =
  let (signal, set) = S.create [] in
  let search_bar = QuickSearchBar.make
      ~number_of_results: 5
      ~search
      ()
  in
  Lwt.async (fun () ->
      let%lwt initial_value = Lwt_list.map_p unserialise initial_value in
      set initial_value;
      Lwt.return_unit
    );
  {signal; set; search_bar; serialise}

let raw_signal s = S.map (List.map s.serialise) s.signal

let signal (s : 'model t) : ('model list, string) Result.t S.t =
  S.map Result.ok s.signal

let clear s =
  s.set [];
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
    ?(make_more_results =
      (Fun.const [] :
         'result ->
       Html_types.tr Html.elt list)
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
  div ~a:[a_class ["form-element"]] [
    label [txt (fst field_name)];

    tablex ~a:[a_class ["container"]] [
      R.tbody (
        Fun.flip S.map s.signal @@ fun elements ->
        List.concat @@ List.mapi
          (fun n element ->
             make_result
               ~classes: ["row"]
               ~onclick: (fun () -> ())
               ~suffix: [
                 td ~a:[a_class ["actions"]] [
                   button
                     ~a: [
                       a_class (if n = List.length elements - 1 then ["disabled"] else []);
                       a_onclick (fun _ -> s.set @@ List.swap n (n+1) @@ S.value s.signal; true);
                     ]
                     [i ~a:[a_class ["material-symbols-outlined"]] [txt "keyboard_arrow_down"]];
                   button
                     ~a: [
                       a_class (if n = 0 then ["disabled"] else []);
                       a_onclick (fun _ -> s.set @@ List.swap (n-1) n @@ S.value s.signal; true);
                     ]
                     [i ~a:[a_class ["material-symbols-outlined"]] [txt "keyboard_arrow_up"]];
                   button
                     ~a: [
                       a_onclick (fun _ -> s.set @@ List.remove n @@ S.value s.signal; true);
                       a_class ["btn-danger"];
                     ]
                     [i ~a:[a_class ["material-symbols-outlined"]] [txt "delete"]];
                 ]
               ]
               element
             :: make_more_results element
          )
          elements
      )
    ];

    QuickSearchBar.render
      ~placeholder: ("Add a " ^ snd field_name ^ " (magic search)")
      ~make_result: (fun person ->
          Lwt.return @@ make_result
            ~onclick:(fun () ->
                s.set (S.value s.signal @ [person]);
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
                [
                  create_dialog_content
                    ~on_save:return
                    (S.value (SearchBar.text (QuickSearchBar.search_bar s.search_bar)))
                ]
              in
              Result.iter (fun element ->
                  s.set (S.value s.signal @ [element]);
                ) result;
              Lwt.return_unit
            )
          "add_circle" ("Create a new " ^ model_name)
      ]
      s.search_bar;
    div ~a:[a_class ["message-box"]] [];
  ]
