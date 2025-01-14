open Nes
open Js_of_ocaml
open Dancelor_client_html

let unique =
  let counter = ref 0 in
  fun () ->
    incr counter;
    "choices-" ^ string_of_int !counter

type 'value choice = {
  id: string;
  value: 'value;
  checked: bool;
  contents: Html_types.label_content_fun elt list;
}

let choice ?(checked = false) ~value contents = {id = unique (); value; checked; contents}

let choice' ?checked ?value contents = choice ?checked ~value contents

type 'value t = {
  box: Html_types.div elt;
  values: 'value S.t;
}

let signal c = c.values
let value c = S.value (signal c)
let render c = c.box

let make_gen_unsafe
    ?name: nam
    ?(has_interacted = S.const false)
    ~validate
    ~post_validate
    ~radios
    choices
  =
  ignore has_interacted;
  (* FIXME *)
  let name = unique () in
  let gather_values_such_that p = List.filter_map (fun choice -> if p choice then Some choice.value else None) choices in
  let (values, set_values) = S.create (gather_values_such_that @@ fun choice -> choice.checked) in
  let values = S.map validate values in
  let update_values () =
    let choice_inputElement choice = Option.get @@ Dom_html.getElementById_coerce choice.id Dom_html.CoerceTo.input in
    set_values @@ gather_values_such_that @@ fun choice -> Js.to_bool (choice_inputElement choice)##.checked
  in
  let box =
    div
      ~a: [a_class ["form-element"]]
      [
        label (Option.to_list (Option.map txt nam));
        div
          ~a: [
            R.a_class
              (
                Fun.flip S.map values @@ function
                | Ok _ -> ["choices"]
                | Error _ -> ["choices"; "invalid"]
              );
            a_onchange (fun _ -> update_values (); true);
          ]
          (
            Fun.flip List.concat_map choices @@ fun choice ->
            [
              input
                ~a: (
                  List.filter_map
                    Fun.id
                    [
                      Some (a_input_type (if radios then `Radio else `Checkbox));
                      Some (a_name name);
                      Some (a_id choice.id);
                      (if choice.checked then Some (a_checked ()) else None);
                    ]
                )
                ();
              label ~a: [a_label_for choice.id] choice.contents;
            ]
          );
        R.div
          ~a: [a_class ["message-box"]]
          (
            Fun.flip S.map values @@ function
            | Ok _ -> []
            | Error msg -> [txt msg]
          )
      ]
  in
  {box; values = S.map post_validate values}

let make_radios_gen ?name ?has_interacted ~validate ~post_validate choices =
  make_gen_unsafe
    ?name
    ~radios: true
    choices
    ?has_interacted
    ~validate: (function
        | [] -> validate None
        | [x] -> validate x
        | _ -> Error "Cannot select multiple options" (* should never happen *)
      )
    ~post_validate

let make_radios ?name ?has_interacted choices =
  make_radios_gen ?name ?has_interacted ~validate: Result.ok ~post_validate: Result.get_ok choices

let make_radios' = make_radios_gen ~post_validate: Fun.id

let make_checkboxes choices =
  make_gen_unsafe
    ~radios: false
    choices
    ~validate: Result.ok
    ~post_validate: Result.get_ok
