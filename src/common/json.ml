open ExtPervasives

type value = Ezjsonm.value
type t = [ `O of (string * value) list ]

let of_value = function
  | `O fields -> `O fields
  | _ -> failwith "Dancelor_common.Json.of_value"

let to_value = function
  | `O fields -> `O fields

let add_field key value = function
  | `O fields ->
     if  not (List.mem_assoc key fields) then
       `O ((key, value) :: fields)
     else
       failwith "Dancelor_common.Json.add_field"

let add_field key value =
  of_value
  ||> add_field key value
  ||> to_value

let add_fields fields json =
  List.fold_left
    (fun json (field, value) ->
      add_field field value json)
    json
    fields

let map_field key fun_ = function
  | `O fields when List.mem_assoc key fields ->
     `O
       (List.map
          (fun (key', value) ->
            (key',
             if key = key'
             then fun_ value
             else value))
          fields)
  | _ -> failwith "Dancelor_common.Json.map_field"
