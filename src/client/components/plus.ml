open Nes
open Html

let prepare (type value)(type component_raw_value)
  ~label
  (components : (value, component_raw_value) Component.s list)
  : (value, int option * component_raw_value list) Component.s
= (module struct
  let label = label

  type nonrec value = value

  (* which component is selected, if any, and a save for each of them *)
  type raw_value = int option * component_raw_value list

  let raw_value_to_yojson (selected, values) =
    `Assoc [
      "selected", (match selected with None -> `Null | Some n -> `Int n);
      "values",
      `List (
        List.map2
          (fun ((module C): (value, component_raw_value) Component.s) value ->
            C.raw_value_to_yojson value
          )
          components
          values
      )
    ]

  let raw_value_of_yojson = function
    | `Assoc [("selected", selected); ("values", `List values)] ->
      let selected = match selected with `Null -> Ok None | `Int n -> Ok (Some n) | _ -> Error "Invalid JSON for selected" in
      let values =
        Result.map List.rev @@
          List.fold_left2
            (fun acc ((module C): (value, component_raw_value) Component.s) value ->
              Result.bind acc @@ fun acc ->
              Result.bind (C.raw_value_of_yojson value) @@ fun value ->
              Result.ok (value :: acc)
            )
            (Ok [])
            components
            values
      in
      Result.bind selected @@ fun selected ->
      Result.bind values @@ fun values ->
      Ok (selected, values)
    | _ -> Error "Invalid JSON format for raw_value"

  let empty_value = (None, List.map (fun ((module C): (value, component_raw_value) Component.s) -> C.empty_value) components)

  let raw_value_from_initial_text text =
    (None, List.map (fun ((module C): (value, component_raw_value) Component.s) -> C.raw_value_from_initial_text text) components)

  type t = {
    choices: (int, string) Component.t;
    initialised_components: (value, component_raw_value) Component.t list; (* NOTE: mind the [t] *)
    inner_html: 'a. ([> Html_types.div] as 'a) elt;
  }

  let selected l = S.map Result.to_option (Component.signal l.choices)

  let signal l =
    S.bind (selected l) @@ function
      | None -> S.const @@ Error ("You must select a " ^ String.lowercase_ascii label ^ ".")
      | Some selected ->
        let (component : (value, component_raw_value) Component.t) =
          List.nth l.initialised_components selected
        in
        Component.signal component

  let raw_signal l =
    S.bind (selected l) @@ fun selected ->
    let component_raw_signals =
      List.map
        (fun (component : (value, component_raw_value) Component.t) ->
          Component.raw_signal component
        )
        l.initialised_components
    in
    S.bind (S.all component_raw_signals) @@ fun component_raw_values ->
    S.const (selected, component_raw_values)

  let focus _ = () (* FIXME *)
  let trigger = focus
  let set _ _ = () (* FIXME *)

  let clear l =
    Component.clear l.choices;
    List.iter Component.clear l.initialised_components

  let inner_html l = l.inner_html

  let make (initial_selected, initial_values) =
    let (initial_selected, initial_values) =
      if List.length initial_values <> List.length components then
        empty_value
      else
          (initial_selected, initial_values)
    in
    let initialised_components = List.map2 Component.initialise components initial_values in
    let choices =
      Choices.make_radios'
        ~label
        ~validate: (Option.to_result ~none: "You must make a choice.")
        (
          List.mapi
            (fun n ((module C): (value, component_raw_value) Component.s) ->
              Choices.choice' ~value: n [txt C.label] ~checked: (Some n = initial_selected)
            )
            components
        )
    in
    let inner_html =
      div [
        Component.inner_html choices;
        R.div ~a: [a_class ["ps-2"; "mt-1"; "border-start"]] (
          flip S.map (Component.signal choices) @@ function
            | Error _ -> []
            | Ok n -> [Component.inner_html @@ List.nth initialised_components n]
        );
      ]
    in
      {choices; initialised_components; inner_html}
end)

let make (type value)(type component_raw_value)
    ~label
    (components : (value, component_raw_value) Component.s list)
    (initial_values : int option * component_raw_value list)
    : (value, int option * component_raw_value list) Component.t
  =
  Component.initialise (prepare ~label components) initial_values

let wrap (type value1)(type value2)(type raw_value1)(type raw_value2)
  (wrap_value : value1 -> value2)
  (wrap_raw_value : raw_value1 -> raw_value2)
  (unwrap_raw_value : raw_value2 -> raw_value1 option)
  ((module C): (value1, raw_value1) Component.s)
  : (value2, raw_value2) Component.s
= (module struct
  include C
  type value = value2
  type raw_value = raw_value2
  let raw_value_to_yojson = C.raw_value_to_yojson % Option.get % unwrap_raw_value
  let raw_value_of_yojson = Result.map wrap_raw_value % C.raw_value_of_yojson
  let empty_value = wrap_raw_value empty_value
  let raw_value_from_initial_text = wrap_raw_value % C.raw_value_from_initial_text
  let signal = S.map (Result.map wrap_value) % signal
  let raw_signal = S.map wrap_raw_value % raw_signal
  let set _ _ = () (* FIXME *)
  let make initial_value =
    match unwrap_raw_value initial_value with
    | Some initial_value -> make initial_value
    | None -> assert false
end)
