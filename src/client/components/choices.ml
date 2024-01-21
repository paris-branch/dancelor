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
  let (values, set_values) = S.create None in
  let box =
    div
      ~a:[
        a_class ["choices"];
        a_onchange (fun event ->
            (
              let open Js_of_ocaml in
              Js.Opt.iter event##.target @@ fun target ->
              let id = target##.id in
              let {value;_} = List.find (fun choice -> id = Js.string choice.id) choices in
              set_values value
            );
            false
          );
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
  {box; values}
