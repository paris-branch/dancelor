let add_field field value = function
  | `O fields when not (List.mem_assoc field fields) ->
     `O ((field, value) :: fields)
  | _ -> failwith "Dancelor_common.YamlHelpers.add_field"

let remove_field field = function
  | `O fields when List.mem_assoc field fields ->
     `O (List.remove_assoc field fields)
  | _ -> failwith "Dancelor_common.YamlHelpers.remove_field"
