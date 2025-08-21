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
}

let choice ?(checked = false) ~value contents = {id = unique (); value; checked; contents}

let choice' ?checked ?value contents = choice ?checked ~value contents

let prepare_gen_unsafe (type value)(type choice_value)
  ~(label : string)
  ~(validate : choice_value list -> (value, string) result)
  ~(radios_or_checkboxes : [`Radio | `Checkbox])
  (choices : choice_value choice list)
  : (value, string) Component.s
= (module struct
  let label = label

  type nonrec value = value
  type state = string [@@deriving yojson]

  let empty = ""
  let from_initial_text _ = ""
  let serialise _ = lwt "" (* FIXME *)

  type t = {
    inner_html: 'a. ([> Html_types.div] as 'a) elt;
    values: choice_value list S.t;
  }

  let state _ = S.const "" (* FIXME: handle state *)
  let signal c = S.map validate c.values
  let inner_html c = c.inner_html
  let actions _ = S.const []

  let focus _ = () (* FIXME *)
  let trigger _ = () (* FIXME *)
  let clear _ = () (* FIXME *)
  let set _ _ = () (* FIXME *)

  (* Helper returning a list of the values held by choices satisfying a certain
     predicate, eg. “this choice is checked”. *)
  let gather_values_such_that p =
    List.filter_map
      (fun choice -> if p choice then Some choice.value else None)
      choices

  let initialise _initial_value =
    (* FIXME: process initial value *)
    let html_name = unique ~name: (Slug.slugify label) () in
    let (values, set_values) = S.create (gather_values_such_that @@ fun choice -> choice.checked) in
    let update_values () =
      let choice_inputElement choice = Option.get @@ Dom_html.getElementById_coerce choice.id Dom_html.CoerceTo.input in
      set_values @@ gather_values_such_that @@ fun choice -> Js.to_bool (choice_inputElement choice)##.checked
    in
    let inner_html =
      div
        ~a: [a_onchange (fun _ -> update_values (); true)]
        (
          List.flatten @@
          (* NOTE: [intersperse \[txt " "\]] wouldn't work because it would be
             the exact same Dom element. *)
          List.interspersei (fun _ -> [txt " "]) @@
          flip List.map choices @@ fun choice ->
          [
            input
              ~a: (
                List.filter_map Fun.id [
                  Some (a_input_type radios_or_checkboxes);
                  Some (a_name html_name);
                  Some (a_id choice.id);
                  Some (a_class ["btn-check"]);
                  (if List.length choices = 1 then Some (a_tabindex (-1)) else None);
                  (if choice.checked then Some (a_checked ()) else None);
                ]
              )
              ();
            Html.label
              ~a: [a_class ["btn"; "btn-outline-secondary"]; a_label_for choice.id]
              choice.contents;
          ]
        )
    in
    lwt {inner_html; values}
end)

let prepare_checkboxes ~label choices =
  prepare_gen_unsafe ~label ~radios_or_checkboxes: `Checkbox ~validate: ok choices

let make_checkboxes ~label choices =
  Component.initialise (prepare_checkboxes ~label choices) "FIXME"

let prepare_radios' ~label ~validate choices =
  prepare_gen_unsafe
    ~label
    ~radios_or_checkboxes: `Radio
    ~validate: (function
      | [] -> validate None
      | [x] -> validate x
      | _ -> assert false (* because of [`Radio], this should never happen *)
    )
    choices

let make_radios' ~label ~validate choices =
  Component.initialise (prepare_radios' ~validate ~label choices) "FIXME"

let make_radios ~label choices =
  make_radios' ~label ~validate: ok choices

(* FIXME: In other components, ~validate is called ~validator *)
