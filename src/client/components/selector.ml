open Nes
open Common

open Html

(* TODO: Filter out from search an element that has already been selected. *)
(* TODO: Also store the search text in the raw signal. *)

type one
type many
type 'any arity = One | Many
let one : one arity = One
let many : many arity = Many

type ('arity, 'model) t = {
  signal: 'model Entry.t list S.t;
  set: 'model Entry.t list -> unit;
  quick_search: 'model Entry.t Search.Quick.t;
  serialise: 'model Entry.t -> 'model Slug.t;
  arity: 'arity arity; (** Whether this selector should select exactly one element. *)
}

let make ~arity ~search ~serialise ~unserialise (_fixme_unused_initial_text, initial_value) =
  let (signal, set) = S.create [] in
  let quick_search = Search.Quick.make ~search () in
  Lwt.async (fun () ->
      let%lwt initial_value = Lwt_list.map_p unserialise initial_value in
      set initial_value;
      Lwt.return_unit
    );
  {signal; set; quick_search; serialise; arity}

let raw_signal s = S.map (fun elts -> ("fixme-unused", List.map s.serialise elts)) s.signal

let signal (s : ('arity, 'model) t) : ('model Entry.t list, string) Result.t S.t =
  Fun.flip S.map s.signal @@ function
  | [x] -> Ok [x]
  | [] when s.arity = One -> Error "You must select an element."
  | _ when s.arity = One -> Error "You must select exactly one element."
  | xs -> Ok xs

let signal_one (s : (one, 'model) t) : ('model Entry.t, string) Result.t S.t =
  assert (s.arity = One);
  S.map (Result.map List.hd) (signal s)

let signal_many (s : (many, 'model) t) : ('model Entry.t list, 'bottom) Result.t S.t =
  assert (s.arity = Many);
  S.map (Result.ok % Result.get_ok) (signal s)

let case_errored ~no ~yes state =
  Fun.flip S.map (signal state) @@ function
  | Error msg -> yes msg
  | _ -> no

let clear s = s.set []

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
      Page.t
     )
    s
  =
  div
    ~a: [a_class ["mb-2"]]
    [
      label [txt field_name];
      tablex
        ~a: [a_class ["table"; "table-borderless"; "table-sm"; "m-0"]]
        [
          R.tbody
            (
              Fun.flip S.map s.signal @@ fun elements ->
              List.map Utils.ResultRow.to_clickable_row @@
              List.concat @@
              List.mapi
                (fun n element ->
                   make_result
                     ~suffix: [
                       Utils.ResultRow.cell
                         ~a: [a_class ["btn-group"; "p-0"]]
                         [
                           button
                             ~a: [
                               a_class (["btn"; "btn-outline-secondary"] @ (if n = List.length elements - 1 then ["disabled"] else []));
                               a_onclick (fun _ -> s.set @@ List.swap n (n + 1) @@ S.value s.signal; true);
                             ]
                             [i ~a: [a_class ["bi"; "bi-arrow-down"]] []];
                           button
                             ~a: [
                               a_class (["btn"; "btn-outline-secondary"] @ (if n = 0 then ["disabled"] else []));
                               a_onclick (fun _ -> s.set @@ List.swap (n - 1) n @@ S.value s.signal; true);
                             ]
                             [i ~a: [a_class ["bi"; "bi-arrow-up"]] []];
                           button
                             ~a: [
                               a_onclick (fun _ -> s.set @@ List.remove n @@ S.value s.signal; true);
                               a_class ["btn"; "btn-warning"];
                             ]
                             [i ~a: [a_class ["bi"; "bi-trash"]] []];
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
              | [_] when s.arity = One -> ["d-none"]
              | _ -> []
            )
        ]
        [
          (
            let label = (if s.arity = One then "Select" else "Add") ^ " a " ^ model_name in
            let label_processing = (if s.arity = One then "Selecting" else "Adding") ^ " a " ^ model_name ^ "..." in
            div
              ~a: [a_class ["btn-group"; "w-100"]]
              [
                Button.make_icon "search" ~classes: ["btn-light"];
                Button.make
                  ~label
                  ~label_processing
                  ~classes: ["btn-outline-secondary"; "w-100"; "text-start"]
                  ~onclick: (fun () ->
                      let%lwt quick_search_result =
                        Page.open_dialog @@ fun quick_search_return ->
                        Search.Quick.render
                          s.quick_search
                          ~return: quick_search_return
                          ~dialog_title: (S.const label)
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
                                  Lwt.map quick_search_return @@
                                  Page.open_dialog' @@ fun sub_dialog_return ->
                                  create_dialog_content
                                    ~on_save: sub_dialog_return
                                    (S.value (Search.Quick.text s.quick_search))
                                )
                              ();
                          ]
                      in
                      Fun.flip Option.iter quick_search_result (fun r -> s.set (S.value s.signal @ [r]));
                      Lwt.return_unit
                    )
                  ()
              ];
          )
        ];
      R.div
        ~a: [R.a_class (case_errored ~no: ["d-block"; "valid-feedback"] ~yes: (Fun.const ["d-block"; "invalid-feedback"]) s)]
        (
          case_errored ~no: [txt " "] ~yes: (List.singleton % txt) s
        );
    ]
