open Nes
open Js_of_ocaml
open Html

module Make (C : Component.S) : Component.S with
  type value = C.value list
  and type raw_value = C.raw_value list
= struct
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

  let clear l = l.set_components []

  let inner_html l = l.inner_html
  let html l = Component.render ~label ~signal: (signal l) (inner_html l)

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
          C.focus component;
          lwt_unit
        )
        ()
    in
    let button_add_object_dom = To_dom.of_button button_add_object in
    let inner_html =
      div [
        R.div
          ~a: [
            R.a_class (
              S.l2
                (@)
                (S.map (function [] -> [] | _ -> ["mb-2"]) components)
                (S.const ["container"; "text-center"])
            )
          ]
          (
            flip S.map components @@ fun components_ ->
            flip List.mapi components_ @@ fun n component ->
            div ~a: [a_class ["row"; "ps-2"; "border-start"]] [
              div ~a: [a_class ["col"; "p-0"]] [C.inner_html component];
              div ~a: [a_class ["col-auto"; "p-0"]] [
                button
                  ~a: [
                    a_class (["btn"; "btn-outline-secondary"] @ (if n = List.length components_ - 1 then ["disabled"] else []));
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
end

module MakeNonEmpty (C : Component.S) : Component.S with
  type value = C.value NonEmptyList.t
  and type raw_value = C.raw_value list
= struct
  include Make(C)
  type value = C.value NonEmptyList.t
  let signal =
    S.map (fun l ->
      Result.bind l @@ Option.to_result ~none: ("You must add at least one " ^ String.lowercase_ascii C.label ^ ".") % NonEmptyList.of_list
    ) %
      signal
  let html l = Component.render ~label ~signal: (signal l) (inner_html l)
end
