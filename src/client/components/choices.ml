open Nes
open Js_of_ocaml
open Dancelor_client_html

let unique =
  let counter = ref 0 in
  fun () ->
    incr counter;
    "choices-" ^ string_of_int !counter

type 'value choice = {
  id : string;
  value : 'value option;
  checked : bool;
  contents : Html_types.label_content_fun elt list;
}

let choice ?(checked=false) ?value contents =
  { id = unique (); value; checked; contents }

type 'value t = {
  box : Html_types.div elt;
  values : 'value option S.t;
}

let signal c = c.values
let render c = c.box

let make choices =
  let name = unique () in
  let (values, set_values) = S.create [] in
  let update_values () =
    let choice_inputElement choice = Option.get @@ Dom_html.getElementById_coerce choice.id Dom_html.CoerceTo.input in
    set_values @@ List.filter_map (fun choice -> if Js.to_bool (choice_inputElement choice)##.checked then choice.value else None) choices
  in
  let values' = Fun.flip S.map values @@ function [] -> None | [x] -> Some x | _ -> failwith "too many values at once" in
  let box =
    div
      ~a:[
        a_class ["choices"];
        a_onchange (fun _ -> update_values (); true);
      ]
      (
        Fun.flip List.concat_map choices @@ fun choice ->
        [
          input ~a:(List.filter_map Fun.id [
              Some (a_input_type `Radio);
              Some (a_name name);
              Some (a_id choice.id);
              (if choice.checked then Some (a_checked ()) else None);
            ]) ();
          label ~a:[a_label_for choice.id] choice.contents;
        ]
      )
  in
  {box; values=values'}
