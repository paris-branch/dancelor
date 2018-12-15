let check_object = function
  | `O fields -> `O fields
  | _ -> failwith "Dancelor_common.JsonHelpers.check_object"

let add_field field value = function
  | `O fields when not (List.mem_assoc field fields) ->
     `O ((field, value) :: fields)
  | _ -> failwith "Dancelor_common.JsonHelpers.add_field"

let add_fields fields json =
  List.fold_left
    (fun json (field, value) ->
      add_field field value json)
    json
    fields

let remove_field field = function
  | `O fields when List.mem_assoc field fields ->
     `O (List.remove_assoc field fields)
  | _ -> failwith "Dancelor_common.JsonHelpers.remove_field"
