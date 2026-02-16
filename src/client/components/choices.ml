open Nes
open Js_of_ocaml
open Html

let unique =
  let counter = ref 0 in
  fun ?name () ->
    incr counter;
    Option.value name ~default: "anonymous" ^
      ("-" ^ string_of_int !counter)

type 'value choice = {
  id: string;
  value: 'value;
  checked: bool;
  contents: Html_types.label_content_fun elt list;
  onclick: unit -> unit;
}

let choice ?(checked = false) ?(onclick = Fun.id) ~value contents = {id = unique (); value; checked; contents; onclick}

let choice' ?checked ?onclick ?value contents = choice ?checked ?onclick ~value contents

let prepare_gen_unsafe (type value)(type choice_value)
    ~(label : string)
    ~(validate : choice_value list -> (value, string) result)
    ~(unvalidate : value -> choice_value list)
    ~(radios_or_checkboxes : [`Radio | `Checkbox])
    (choices : choice_value choice list)
    : (value, bool list) Component.s
  =
  if List.contains_duplicates (List.map (fun choice -> choice.value) choices) then
    invalid_arg "Components.Choices: choices must not contain duplicates";
  if radios_or_checkboxes = `Radio && List.length (List.filter (fun choice -> choice.checked) choices) > 1 then
    invalid_arg "Components.Choices: radios must contain at most one checked choice";
  (module struct
    let label = label

    type nonrec value = value
    type state = bool list [@@deriving yojson]

    let empty = List.map (fun choice -> choice.checked) choices
    let from_initial_text _ = empty
    let value_to_string _ = lwt "<FIXME choices>"

    let value_to_state value =
      let choice_values = unvalidate value in
      lwt @@ List.map (fun choice -> List.mem choice.value choice_values) choices

    type t = {
      inner_html: 'a. ([> Html_types.div] as 'a) elt;
      state: bool list S.t;
      set_state: bool list -> unit;
    }

    let state t = t.state

    let signal t =
      S.bind (state t) @@ fun state ->
      S.const @@
      validate @@
      List.concat @@
      List.map2 (fun choice checked -> if checked then [choice.value] else []) choices state

    let inner_html c = c.inner_html
    let actions _ = S.const []

    let focus _ = ()
    let trigger _ = ()

    let clear t = t.set_state empty; lwt_unit

    let set t value = t.set_state <$> value_to_state value

    let initialise initial_value =
      let html_name = unique ~name: (Slug.slugify label) () in
      let initial_value =
        (* check that the initial value is sane, that is it has the same length as
           the choices, and, in case of radios, it contains at most one checked
           element *)
        if List.length initial_value = List.length choices
          && (
            radios_or_checkboxes = `Checkbox || List.length (List.filter (fun choice -> choice.checked) choices) <= 1
          ) then
          initial_value
        else
            (List.map (fun choice -> choice.checked) choices)
      in
      let (state, set_state) = S.create initial_value in
      let update_values () =
        let choice_inputElement choice = Option.get @@ Dom_html.getElementById_coerce choice.id Dom_html.CoerceTo.input in
        set_state (List.map (fun choice -> Js.to_bool (choice_inputElement choice)##.checked) choices)
      in
      let inner_html =
        div
          ~a: [a_onchange (fun _ -> update_values (); true)]
          (
            List.flatten @@
            (* NOTE: [intersperse \[txt " "\]] wouldn't work because it would be
               the exact same Dom element. *)
            List.interspersei (fun _ -> [txt " "]) @@
            flip List.mapi choices @@ fun i choice ->
            [
              input
                ~a: (
                  List.filter_map Fun.id [
                    Some (a_input_type radios_or_checkboxes);
                    Some (a_name html_name);
                    Some (a_id choice.id);
                    Some (a_class ["btn-check"]);
                    (if List.length choices = 1 then Some (a_tabindex (-1)) else None);
                    (if List.nth initial_value i then Some (a_checked ()) else None);
                  ]
                )
                ();
              Html.label
                ~a: [a_class ["btn"; "btn-light"]; a_label_for choice.id; a_onclick (fun _ -> choice.onclick (); true)]
                choice.contents;
            ]
          )
      in
      lwt {inner_html; set_state; state}
  end)

let prepare_checkboxes ~label choices =
  prepare_gen_unsafe ~label ~radios_or_checkboxes: `Checkbox ~validate: ok ~unvalidate: id choices

let make_checkboxes ~label choices =
  Component.initialise
    (prepare_checkboxes ~label choices)
    (List.map (fun choice -> choice.checked) choices)

let prepare_radios ~label choices =
  prepare_gen_unsafe
    ~label
    ~radios_or_checkboxes: `Radio
    ~validate: (Option.to_result ~none: "You must select something." % List.to_option)
    ~unvalidate: List.singleton
    choices

let make_radios ~label choices =
  Component.initialise
    (prepare_radios ~label choices)
    (List.map (fun choice -> choice.checked) choices)

(* FIXME: In other components, ~validate is called ~validator *)
