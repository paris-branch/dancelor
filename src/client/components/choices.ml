open Dancelor_client_html

let unique =
  let counter = ref 0 in
  fun () ->
    incr counter;
    "choices-" ^ string_of_int !counter

let make choices =
  let name = unique () in
  let (value, set_value) = S.create None in
  let box =
    div
      ~a:[
        a_class ["choices"];
        a_onchange (fun event ->
            (
              let open Js_of_ocaml in
              Js.Opt.iter event##.target @@ fun target ->
              let id = target##.id in
              let (_, value, _, _) = List.find (fun (id', _, _, _) -> id = Js.string id') choices in
              set_value value
            );
            false
          );
      ]
      (
        Fun.flip List.concat_map choices @@ fun (id, _value, checked, content) ->
        [
          input ~a:(List.filter_map Fun.id [
              Some (a_input_type `Radio);
              Some (a_name name);
              Some (a_id id);
              (if checked then Some (a_checked ()) else None);
            ]) ();
          label ~a:[a_label_for id] content;
        ]
      )
  in
  (box, value)

let choice ?value ?(checked=false) content =
  (* FIXME: it would be better as a record, but this would imply managing to
     type the content part of this. *)
  (unique (), value, checked, content)
