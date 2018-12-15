open ExtPervasives

type value = Ezjsonm.value
type t = [ `O of (string * value) list ]

let of_value = function
  | `O fields -> `O fields
  | _ -> failwith "Dancelor_common.Json.of_value"

let to_value = function
  | `O fields -> `O fields

let to_ezjsonm = to_value

let on_value fun_ json =
  to_value (fun_ (of_value json))

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

let from_string = Ezjsonm.from_string ||> of_value
let to_string = function
  | `O fields -> Ezjsonm.to_string ~minify:false (`O fields)

let rec find_opt path json =
  match path, json with
  | [], _ -> Some json
  | key :: path, `O fields when List.mem_assoc key fields ->
     find_opt path (List.assoc key fields)
  | _, _ -> None

let find_opt path json =
  find_opt path (to_value json)

let find path json =
  match find_opt path json with
  | None -> failwith "Dancelor_common.Json.find"
  | Some json -> json

let get_opt ~k path json =
  let open Option in
  find_opt path json >>= fun value ->
  Some (k value)

let get ~k path json =
  match find_opt path json with
  | None -> failwith "Dancelor_common.Json.get"
  | Some value -> k value

let string = function
  | `String s -> s
  | _ -> failwith "Dancelor_common.Json.string"

let int = function
  | `Float f -> int_of_float f
  | _ -> failwith "Dancelor_common.Json.int"

let slug = string ||> Slug.from_string

let strings = function
  | `A values -> List.map string values
  | _ -> failwith "Dancelor_common.Json.strings"

let list cast = function
  | `A values -> List.map cast values
  | _ -> failwith "Dancelor_common.Json.list"
