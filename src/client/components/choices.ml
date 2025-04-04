open Nes
open Js_of_ocaml
open Html

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
  box: 'a. ([> Html_types.div] as 'a) elt;
  values: 'value S.t;
}

let signal c = c.values
let value c = S.value (signal c)
let render c = c.box

let make_gen_unsafe
    ?name: nam
    ~validation
    ~validate
    ~post_validate
    ~radios
    choices
  =
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
      ~a: [a_class ["mb-2"]]
      (
        List.filter_map
          Fun.id
          [
            (
              match nam with
              | None -> None
              | Some nam -> Some (label ~a: [a_class ["form-label"]] [txt nam])
            );
            Some
              (
                div
                  ~a: [a_onchange (fun _ -> update_values (); true)]
                  (
                    Fun.flip List.concat_map choices @@ fun choice ->
                    [
                      input
                        ~a: (
                          List.filter_map
                            Fun.id
                            [
                              Some (a_input_type (if radios then `Radio else `Checkbox));
                              Some
                                (
                                  R.a_class
                                    (
                                      Fun.flip S.map values @@ function
                                      | Ok _ -> ["btn-check"; "form-check-input"; "is-valid"]
                                      | Error _ -> ["btn-check"; "form-check-input"; "is-invalid"]
                                    )
                                );
                              Some (a_name name);
                              Some (a_id choice.id);
                              (if choice.checked then Some (a_checked ()) else None);
                            ]
                        )
                        ();
                      label ~a: [a_class ["btn"; "btn-outline-secondary"; "mx-1"]; a_label_for choice.id] choice.contents;
                    ]
                  )
              );
            (
              match validation with
              | false -> None
              | true ->
                Some
                  (
                    R.div
                      ~a: [
                        R.a_class
                          (
                            Fun.flip S.map values @@ function
                            | Ok _ -> ["d-block"; "valid-feedback"]
                            | Error _ -> ["d-block"; "invalid-feedback"]
                          )
                      ]
                      (
                        Fun.flip S.map values @@ function
                        | Ok _ -> [txt "Looks good!"]
                        | Error msg -> [txt "Error: "; txt msg]
                      )
                  )
            );
          ]
      )
  in
  {box; values = S.map post_validate values}

let make_radios_gen ?name ~validate ~post_validate choices =
  make_gen_unsafe
    ?name
    ~radios: true
    choices
    ~validate: (function
        | [] -> validate None
        | [x] -> validate x
        | _ -> Error "Cannot select multiple options" (* should never happen *)
      )
    ~post_validate

let make_radios ?name choices =
  make_radios_gen ?name ~validation: false ~validate: Result.ok ~post_validate: Result.get_ok choices

let make_radios' = make_radios_gen ~validation: true ~post_validate: Fun.id

let make_checkboxes choices =
  make_gen_unsafe
    ~radios: false
    choices
    ~validation: false
    ~validate: Result.ok
    ~post_validate: Result.get_ok
