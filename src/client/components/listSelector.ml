open Nes
open Js_of_ocaml_tyxml.Tyxml_js
open Dancelor_client_html
module Model = Dancelor_client_model

(* TODO: Filter out from search a deviser that has already been selected. *)

type 'model t = {
  inner_signal : 'model list S.t;
  signal : ('model list, string) Result.t S.t;
  set : 'model list -> unit;
  search_bar : 'model QuickSearchBar.t;
}

let make ~search f =
  let (inner_signal, set) = S.create [] in
  let signal = S.map f inner_signal in
  let search_bar = QuickSearchBar.make
      ~number_of_results: 5
      ~search
      ()
  in
  {inner_signal; signal; set; search_bar}

let signal s = s.signal

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
    s
  =
  div ~a:[a_class ["list-selector"]] [
    tablex ~a:[a_class ["container"]] [
      R.tbody (
        Fun.flip S.map s.inner_signal @@ List.mapi (fun n person ->
            make_result
              ~classes: ["row"]
              ~onclick: (fun () -> ())
              ~suffix: [
                td ~a:[a_class ["actions"]] [
                  button
                    ~a: [a_onclick (fun _ -> s.set @@ List.swap n (n+1) @@ S.value s.inner_signal; true)]
                    [i ~a:[a_class ["material-symbols-outlined"]] [txt "keyboard_arrow_down"]];
                  button
                    ~a: [a_onclick (fun _ -> s.set @@ List.swap (n-1) n @@ S.value s.inner_signal; true)]
                    [i ~a:[a_class ["material-symbols-outlined"]] [txt "keyboard_arrow_up"]];
                  button
                    ~a: [
                      a_onclick (fun _ -> s.set @@ List.remove n @@ S.value s.inner_signal; true);
                      a_class ["btn-danger"];
                    ]
                    [i ~a:[a_class ["material-symbols-outlined"]] [txt "delete"]];
                ]
              ]
              person
          )
      )
    ];
    QuickSearchBar.render
      ~placeholder: "Add a deviser (magic search)"
      ~make_result: (fun person ->
          Lwt.return @@ make_result
            ~onclick:(fun () ->
                s.set (S.value s.inner_signal @ [person]);
                QuickSearchBar.clear s.search_bar;
              )
            ~suffix:[]
            person
        )
      s.search_bar;
    div ~a:[a_class ["message-box"]] [];
  ]
