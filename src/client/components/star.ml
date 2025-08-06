open Nes
open Js_of_ocaml
open Html

let prepare (type value)(type raw_value)
  (component : (value, raw_value) Component.s)
  : (value list, raw_value list) Component.s
= (module struct
  module C = (val component)

  let label = C.label ^ "s"

  type value = C.value list
  type raw_value = C.raw_value list

  let empty_value = []

  type t = {
    components: C.t list S.t;
    set_components: C.t list -> unit;
    inner_html: 'a. ([> Html_types.div] as 'a) elt;
    button_add_object_dom: Dom_html.buttonElement Js.t;
  }

  let signal l =
    S.map (Result.map_error ((^) ("At least one " ^ String.lowercase_ascii C.label ^ " has an error: "))) @@
    S.map (Result.map List.rev) @@
    S.bind l.components @@
    List.fold_left
      (fun values component ->
        RS.bind values @@ fun values ->
        RS.bind (C.signal component) @@ fun value ->
        RS.pure (value :: values)
      )
      (S.const (Ok []))

  let raw_signal l =
    S.map List.rev @@
    S.bind l.components @@
    List.fold_left
      (fun values component ->
        S.bind values @@ fun values ->
        S.bind (C.raw_signal component) @@ fun value ->
        S.const (value :: values)
      )
      (S.const [])

  let focus l =
    match S.value l.components with
    | first :: _ -> C.focus first
    | [] -> l.button_add_object_dom##focus

  let trigger = focus

  let set _ _ = assert false

  let clear l = l.set_components []

  let inner_html l = l.inner_html

  let make initial_values =
    (* NOTE: We use [fun _ _ -> false] because React needs to be able to compare
       things and components are very non-comparable. If it causes issues, then
       we need the module type {!Component} to provide an equality. *)
    let (components, set_components) = S.create ~eq: (fun _ _ -> false) @@ List.map C.make initial_values in
    let button_add_object =
      Button.make
        ~label: ("Add a " ^ String.lowercase_ascii C.label)
        ~label_processing: ("Adding a " ^ String.lowercase_ascii C.label)
        ~icon: "plus-circle"
        ~classes: ["btn-primary"]
        ~onclick: (fun () ->
          let component = C.make C.empty_value in
          set_components (S.value components @ [component]);
          C.trigger component;
          lwt_unit
        )
        ()
    in
    let button_add_object_dom = To_dom.of_button button_add_object in
    let inner_html =
      div [
        R.div
          ~a: [R.a_class (S.map (function [] -> [] | _ -> ["mb-2"]) components)]
          (
            flip S.map components @@ fun components_ ->
            let last_index = List.length components_ - 1 in
            flip List.mapi components_ @@ fun n component ->
            div ~a: [a_class ["row"; "border-start"; "m-0"; "ps-2"; (if n = 0 then "pb-1" else if n = last_index then "pt-1" else "py-1")]] [
              div ~a: [a_class ["col"; "text-start"; "p-0"]] [C.inner_html component];
              div ~a: [a_class ["col-auto"; "p-0"]] [
                button
                  ~a: [
                    a_class (["btn"; "btn-outline-secondary"] @ (if n = last_index then ["disabled"] else []));
                    a_onclick (fun _ -> set_components @@ List.swap n (n + 1) @@ S.value components; true);
                  ]
                  [i ~a: [a_class ["bi"; "bi-arrow-down"]] []];
                button
                  ~a: [
                    a_class (["btn"; "btn-outline-secondary"] @ (if n = 0 then ["disabled"] else []));
                    a_onclick (fun _ -> set_components @@ List.swap (n - 1) n @@ S.value components; true);
                  ]
                  [i ~a: [a_class ["bi"; "bi-arrow-up"]] []];
                button
                  ~a: [
                    a_onclick (fun _ -> set_components @@ List.remove n @@ S.value components; true);
                    a_class ["btn"; "btn-warning"];
                  ]
                  [i ~a: [a_class ["bi"; "bi-trash"]] []];
              ];
            ]
          );
        div [
          Button.clear ~onclick: (fun () -> set_components []) ();
          txt " ";
          button_add_object;
        ];
      ]
    in
      {components; set_components; inner_html; button_add_object_dom}
end)

let make (type value)(type raw_value)
    (component : (value, raw_value) Component.s)
    (initial_values : raw_value list)
    : (value list, raw_value list) Component.t
  =
  Component.initialise (prepare component) initial_values

let prepare_non_empty (type value)(type raw_value)
  (component : (value, raw_value) Component.s)
  : (value NonEmptyList.t, raw_value list) Component.s
= (module struct
  include (val (prepare component))
  module C = (val component)
  type value = C.value NonEmptyList.t
  let signal =
    S.map (fun l ->
      Result.bind l @@ Option.to_result ~none: ("You must add at least one " ^ String.lowercase_ascii C.label ^ ".") % NonEmptyList.of_list
    ) %
      signal
end)

let make_non_empty (type value)(type raw_value)
    (component : (value, raw_value) Component.s)
    (initial_values : raw_value list)
    : (value NonEmptyList.t, raw_value list) Component.t
  =
  Component.initialise (prepare_non_empty component) initial_values
