open Nes
open Js_of_ocaml_tyxml.Tyxml_js
open Dancelor_client_html
module Model = Dancelor_client_model
module Utils = Dancelor_client_utils

(* TODO: Filter out from search an element that has already been selected. *)
(* TODO: Also store the search text in the raw signal. *)

type one
type many
type 'any arity = One | Many
let one : one arity = One
let many : many arity = Many

type ('arity, 'model) t = {
  has_interacted: bool S.t;
  set_interacted: unit -> unit;
  signal: 'model list S.t;
  set: 'model list -> unit;
  search_bar: 'model QuickSearchBar.t;
  serialise: 'model -> 'model Slug.t;
  arity: 'arity arity; (** Whether this selector should select exactly one element. *)
}

let make ~arity ?(has_interacted = S.const false) ~search ~serialise ~unserialise (initial_input, initial_value) =
  let (has_interacted_locally, set_interacted) = S.create false in
  let has_interacted = S.l2 (||) has_interacted has_interacted_locally in
  let set_interacted () =
    set_interacted true;
    S.stop ~strong: true has_interacted
  in
  let (signal, set) = S.create [] in
  let search_bar =
    QuickSearchBar.make
      ~number_of_results: 5
      ~search
      ~initial_input
      ()
  in
  Lwt.async (fun () ->
    let%lwt initial_value = Lwt_list.map_p unserialise initial_value in
    set initial_value;
    Lwt.return_unit
  );
  {has_interacted; set_interacted; signal; set; search_bar; serialise; arity}

let raw_signal s =
  S.bind (QuickSearchBar.text s.search_bar) @@ fun input ->
  S.bind s.signal @@ fun elements ->
  S.const (input, List.map s.serialise elements)

let signal (s : ('arity, 'model) t) : ('model list, string) Result.t S.t =
  Fun.flip S.map s.signal @@ function
    | [x] -> Ok [x]
    | [] when s.arity = One -> Error "You must select an element."
    | _ when s.arity = One -> Error "You must select exactly one element."
    | xs -> Ok xs

let signal_one (s : (one, 'model) t) : ('model, string) Result.t S.t =
  assert (s.arity = One);
  S.map (Result.map List.hd) (signal s)

let signal_many (s : (many, 'model) t) : ('model list, 'bottom) Result.t S.t =
  assert (s.arity = Many);
  S.map (Result.ok % Result.get_ok) (signal s)

let has_interacted state = state.has_interacted

let case_errored ~no ~yes state =
  S.bind (has_interacted state) @@ fun has_interacted ->
  Fun.flip S.map (signal state) @@ function
    | Error msg when has_interacted -> yes msg
    | _ -> no

let clear s =
  s.set [];
  QuickSearchBar.clear s.search_bar

let render
    ~(make_result :
      ?classes: string list ->
      ?action: Utils.ResultRow.action ->
      ?prefix: Utils.ResultRow.cell list ->
      ?suffix: Utils.ResultRow.cell list ->
      'result ->
      Utils.ResultRow.t
    )
    ?(make_more_results =
    (Fun.const []: 'result ->
      Utils.ResultRow.t list))
    ~field_name
    ~model_name
    ~(create_dialog_content :
      ?on_save: ('result -> unit) ->
      string ->
      Html_types.div Html.elt
    )
    s
  =
  div
    ~a: [
      R.a_class (case_errored ~no: ["form-element"] ~yes: (Fun.const ["form-element"; "invalid"]) s);
    ]
    [
      label [txt (fst field_name)];
      tablex
        ~a: [a_class ["container"]]
        [
          R.tbody
            (
              Fun.flip S.map s.signal @@ fun elements ->
              List.map Utils.ResultRow.to_clickable_row @@
              List.concat @@
              List.mapi
                (fun n element ->
                  make_result
                    ~classes: ["row"]
                    ~suffix: [
                      Utils.ResultRow.cell
                        ~a: [a_class ["actions"]]
                        [
                          button
                            ~a: [
                              a_class (if n = List.length elements - 1 then ["disabled"] else []);
                              a_onclick (fun _ -> s.set @@ List.swap n (n + 1) @@ S.value s.signal; true);
                            ]
                            [i ~a: [a_class ["material-symbols-outlined"]] [txt "keyboard_arrow_down"]];
                          button
                            ~a: [
                              a_class (if n = 0 then ["disabled"] else []);
                              a_onclick (fun _ -> s.set @@ List.swap (n - 1) n @@ S.value s.signal; true);
                            ]
                            [i ~a: [a_class ["material-symbols-outlined"]] [txt "keyboard_arrow_up"]];
                          button
                            ~a: [
                              a_onclick (fun _ -> s.set @@ List.remove n @@ S.value s.signal; true);
                              a_class ["btn-danger"];
                            ]
                            [i ~a: [a_class ["material-symbols-outlined"]] [txt "delete"]];
                        ]
                    ]
                    element :: make_more_results element
                )
                elements
            )
        ];
      div
        ~a: [
          R.a_class
            (
              Fun.flip S.map s.signal @@ function
                | [_] when s.arity = One -> ["hidden"]
                | _ -> []
            )
        ]
        [
          QuickSearchBar.render
            ~placeholder: ((if s.arity = One then "Select" else "Add") ^ " a " ^ snd field_name ^ " (magic search)")
            ~on_focus: s.set_interacted
            ~make_result: (fun ?classes person ->
              make_result
                ?classes
                ~action: (
                  Utils.ResultRow.callback @@ fun () ->
                  s.set (S.value s.signal @ [person]);
                  QuickSearchBar.clear s.search_bar;
                )
                ~suffix: []
                person
            )
            ~more_lines: [
              Utils.ResultRow.icon_row
                ~action: (
                  Utils.ResultRow.callback @@ fun () ->
                  Lwt.async @@ fun () ->
                  let%lwt result =
                    Dialog.open_ @@ fun return ->
                    QuickSearchBar.clear s.search_bar;
                    [
                      create_dialog_content
                        ~on_save: return
                        (S.value (SearchBar.text (QuickSearchBar.search_bar s.search_bar)))
                    ]
                  in
                  Result.iter
                    (fun element ->
                      s.set (S.value s.signal @ [element]);
                    )
                    result;
                  Lwt.return_unit
                )
                "add_circle"
                ("Create a new " ^ model_name)
            ]
            s.search_bar;
        ];
      R.div
        ~a: [a_class ["message-box"]]
        (
          case_errored ~no: [] ~yes: (List.singleton % txt) s
        );
    ]
