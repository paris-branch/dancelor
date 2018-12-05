type json = Ezjsonm.value
type json_object = [`O of (string * json) list]

let add_field key value = function
  | `O l when not (List.mem_assoc key l) ->
     `O ((key, value) :: l)
  | _ ->
     raise (Invalid_argument "JsonHelpers.add_field")
