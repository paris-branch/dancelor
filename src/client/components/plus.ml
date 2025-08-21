open Nes
open Html

exception PartialBecauseWrapped of string
let partial_because_wrapped s = raise (PartialBecauseWrapped s)

let prepare (type value)(type component_state)
  ~label
  (components : (value, component_state) Component.s list)
  : (value, int option * component_state list) Component.s
= (module struct
  let label = label

  type nonrec value = value

  (* which component is selected, if any, and a save for each of them *)
  type state = int option * component_state list

  let state_to_yojson (selected, values) =
    `Assoc [
      "selected", (match selected with None -> `Null | Some n -> `Int n);
      "values",
      `List (
        List.map2
          (fun ((module C): (value, component_state) Component.s) value ->
            C.state_to_yojson value
          )
          components
          values
      )
    ]

  let state_of_yojson = function
    | `Assoc [("selected", selected); ("values", `List values)] ->
      let selected = match selected with `Null -> Ok None | `Int n -> Ok (Some n) | _ -> Error "Invalid JSON for selected" in
      let values =
        Result.map List.rev @@
          List.fold_left2
            (fun acc ((module C): (value, component_state) Component.s) value ->
              Result.bind acc @@ fun acc ->
              Result.bind (C.state_of_yojson value) @@ fun value ->
              Result.ok (value :: acc)
            )
            (Ok [])
            components
            values
      in
      Result.bind selected @@ fun selected ->
      Result.bind values @@ fun values ->
      Ok (selected, values)
    | _ -> Error "Invalid JSON format for state"

  let empty_component_states = List.map (fun ((module C): (value, component_state) Component.s) -> C.empty) components
  let empty = (None, empty_component_states)

  let from_initial_text text =
    (None, List.map (fun ((module C): (value, component_state) Component.s) -> C.from_initial_text text) components)

  (* Because they all have the same type, we cannot know which one the value
     comes from. So we try them all and take the first serialisation that works.
     According to its type, [serialise] should be a total function. However,
     because of {!wrap}, that is not always the case. FIXME: This is disgusting,
     we really need to get this component type-safe. *)
  let serialise value =
    Option.get
    <$> Lwt_list.find_mapi_s
        (fun n ((module C): (value, component_state) Component.s) ->
          try
            let%lwt value = C.serialise value in
            lwt_some (Some n, snd @@ List.replace n value empty_component_states)
          with
            | PartialBecauseWrapped _ -> lwt_none
        )
        components

  type t = {
    choices: (int, string) Component.t;
    initialised_components: (value, component_state) Component.t list; (* NOTE: mind the [t] *)
    inner_html: 'a. ([> Html_types.div] as 'a) elt;
  }

  let selected l = S.map Result.to_option (Component.signal l.choices)

  let signal l =
    S.bind (selected l) @@ function
      | None -> S.const @@ Error ("You must select a " ^ String.lowercase_ascii label ^ ".")
      | Some selected ->
        let (component : (value, component_state) Component.t) =
          List.nth l.initialised_components selected
        in
        Component.signal component

  let state l =
    S.bind (selected l) @@ fun selected ->
    let component_states =
      List.map
        (fun (component : (value, component_state) Component.t) ->
          Component.state component
        )
        l.initialised_components
    in
    S.bind (S.all component_states) @@ fun component_states ->
    S.const (selected, component_states)

  let focus _ = () (* FIXME *)
  let trigger = focus
  let set _ _ = () (* FIXME *)

  let clear l =
    Component.clear l.choices;
    List.iter Component.clear l.initialised_components

  let inner_html l = l.inner_html
  let actions _ = S.const []

  let initialise (initial_selected, initial_values) =
    let (initial_selected, initial_values) =
      if List.length initial_values <> List.length components then
        empty
      else
          (initial_selected, initial_values)
    in
    let%lwt initialised_components =
      Lwt_list.map_s (uncurry Component.initialise) (List.combine components initial_values)
    in
    let%lwt choices =
      Choices.make_radios'
        ~label
        ~validate: (Option.to_result ~none: "You must make a choice.")
        (
          List.mapi
            (fun n ((module C): (value, component_state) Component.s) ->
              Choices.choice' ~value: n [txt C.label] ~checked: (Some n = initial_selected)
            )
            components
        )
    in
    let inner_html =
      div [
        div ~a: [a_class ["row"]] [
          div ~a: [a_class ["col"]] [Component.inner_html choices];
          R.div ~a: [a_class ["col-auto"]] (
            S.bind (Component.signal choices) @@ function
              | Error _ -> S.const []
              | Ok n -> Component.actions @@ List.nth initialised_components n
          );
        ];
        R.div ~a: [a_class ["ps-2"; "mt-1"; "border-start"]] (
          flip S.map (Component.signal choices) @@ function
            | Error _ -> []
            | Ok n -> [Component.inner_html @@ List.nth initialised_components n]
        );
      ]
    in
    lwt {choices; initialised_components; inner_html}
end)

let make (type value)(type component_state)
    ~label
    (components : (value, component_state) Component.s list)
    (initial_values : int option * component_state list)
    : (value, int option * component_state list) Component.t Lwt.t
  =
  Component.initialise (prepare ~label components) initial_values

let wrap (type value1)(type value2)(type state1)(type state2)
  (wrap_value : value1 -> value2)
  (unwrap_value : value2 -> value1 option)
  (wrap_state : state1 -> state2)
  (unwrap_state : state2 -> state1 option)
  ((module C): (value1, state1) Component.s)
  : (value2, state2) Component.s
= (module struct
  include C
  type value = value2
  type state = state2
  let state_to_yojson = C.state_to_yojson % Option.value' ~default: (fun () -> partial_because_wrapped "state_to_yojson") % unwrap_state
  let state_of_yojson = Result.map wrap_state % C.state_of_yojson
  let empty = wrap_state empty
  let from_initial_text = wrap_state % C.from_initial_text
  let serialise = wrap_state <%> serialise % Option.value' ~default: (fun () -> partial_because_wrapped "serialise") % unwrap_value
  let signal = S.map (Result.map wrap_value) % signal
  let state = S.map wrap_state % state
  let set _ _ = () (* FIXME *)
  let initialise initial_value =
    match unwrap_state initial_value with
    | Some initial_value -> initialise initial_value
    | None -> assert false
end)
