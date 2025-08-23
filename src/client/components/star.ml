open Nes
open Js_of_ocaml
open Html

let prepare (type value)(type state)
  ~label
  ?(more_actions = S.const [])
  ((module C): (value, state) Component.s)
  : (value list, state list) Component.s
= (module struct
  let label = label

  type value = C.value list
  type state = C.state list [@@deriving yojson]

  let empty = []
  let from_initial_text = List.singleton % C.from_initial_text

  let value_to_state = Lwt_list.map_p C.value_to_state

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

  let state l =
    S.map List.rev @@
    S.bind l.components @@
    List.fold_left
      (fun values component ->
        S.bind values @@ fun values ->
        S.bind (C.state component) @@ fun value ->
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
  let actions _ = more_actions

  let initialise initial_values =
    let%lwt components = Lwt_list.map_p C.initialise initial_values in
    (* NOTE: We use [fun _ _ -> false] because React needs to be able to compare
       things and components are very non-comparable. If it causes issues, then
       we need the module type {!Component} to provide an equality. *)
    let (components, set_components) = S.create ~eq: (fun _ _ -> false) components in
    let button_add_object =
      Utils.Button.make
        ~label: ("Add a " ^ String.lowercase_ascii C.label)
        ~label_processing: ("Adding a " ^ String.lowercase_ascii C.label)
        ~icon: "plus-circle"
        ~classes: ["btn-secondary"]
        ~onclick: (fun () ->
          let%lwt component = C.initialise C.empty in
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
              R.div ~a: [a_class ["col-auto"; "p-0"]] (
                S.l2
                  (@)
                  (C.actions component)
                  (
                    S.const [
                      Utils.Button.make
                        ~icon: "trash"
                        ~tooltip: ("Remove this " ^ String.lowercase_ascii C.label ^ " from the list. It cannot be recovered.")
                        ~classes: ["btn-warning"]
                        ~onclick: (fun _ -> set_components @@ List.remove n @@ S.value components; lwt_unit)
                        ();
                      Utils.Button.make
                        ~icon: "arrow-down"
                        ~tooltip: ("Move this " ^ String.lowercase_ascii C.label ^ " down in the list.")
                        ~classes: (["btn-outline-secondary"] @ (if n = last_index then ["disabled"] else []))
                        ~onclick: (fun _ -> set_components @@ List.swap n (n + 1) @@ S.value components; lwt_unit)
                        ();
                      Utils.Button.make
                        ~icon: "arrow-up"
                        ~tooltip: ("Move this " ^ String.lowercase_ascii C.label ^ " up in the list.")
                        ~classes: (["btn"; "btn-outline-secondary"] @ (if n = 0 then ["disabled"] else []))
                        ~onclick: (fun _ -> set_components @@ List.swap (n - 1) n @@ S.value components; lwt_unit)
                        ();
                    ]
                  )
              );
            ]
          );
        div [button_add_object];
      ]
    in
    lwt {components; set_components; inner_html; button_add_object_dom}
end)

let make (type value)(type state)
    ~label
    ?more_actions
    (component : (value, state) Component.s)
    (initial_values : state list)
    : (value list, state list) Component.t Lwt.t
  =
  Component.initialise (prepare ~label ?more_actions component) initial_values

let prepare_non_empty (type value)(type state)
  ~label
  ?more_actions
  ((module C): (value, state) Component.s)
  : (value NEList.t, state list) Component.s
= (module struct
  include (val (prepare ~label ?more_actions (module C)))
  type value = C.value NEList.t
  let value_to_state = value_to_state % NEList.to_list
  let set c v = set c @@ NEList.to_list v
  let signal =
    S.map (fun l ->
      Result.bind l @@ Option.to_result ~none: ("You must add at least one " ^ String.lowercase_ascii C.label ^ ".") % NEList.of_list
    ) %
      signal
end)

let make_non_empty (type value)(type state)
    ~label
    ?more_actions
    (component : (value, state) Component.s)
    (initial_values : state list)
    : (value NEList.t, state list) Component.t Lwt.t
  =
  Component.initialise (prepare_non_empty ~label ?more_actions component) initial_values
