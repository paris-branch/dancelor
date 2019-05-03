open NesPervasives

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
  | `O fields when not (List.mem_assoc key fields) ->
    `O ((key, value) :: fields)
  | _ ->
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

let remove_field key = function
  | `O fields when List.mem_assoc key fields ->
    `O (List.remove_assoc key fields)
  | _ ->
    failwith "Dancelor_common.Json.remove_field"

let rec yojson_to_jsonm = function
  | `Null -> `Null
  | `Bool b -> `Bool b
  | `Int i -> `Float (float_of_int i)
  | `Float f -> `Float f
  | `String s -> `String s
  | `Assoc kjl ->
    `O (List.map (fun (k, j) -> (k, yojson_to_jsonm j)) kjl)
  | `List jl ->
    `A (List.map yojson_to_jsonm jl)

(* let rec jsonm_to_yojson = function
 *   | `Null -> `Null
 *   | `Bool b -> `Bool b
 *   | `Float f -> `Float f
 *   | `String s -> `String s
 *   | `A jl ->
 *      `List (List.map jsonm_to_yojson jl)
 *   | `O kjl ->
 *      `Assoc (List.map (fun (k, j) -> (k, jsonm_to_yojson j))) *)

let from_string = Yojson.Basic.from_string ||> yojson_to_jsonm ||> of_value
let to_string = function
  | `O fields -> Ezjsonm.to_string ~minify:false (`O fields)

let path_to_string =
  to_string_of_pp (NesList.pp ~sep: " > " NesString.pp)

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
  | None -> failwith ("Dancelor_common.Json.find: couldn't find: " ^ path_to_string path)
  | Some json -> json

let get_opt ~k path json =
  let open NesOption in
  find_opt path json >>= fun value ->
  k value

let get ~k path json =
  match get_opt ~k path json with
  | None -> failwith ("Dancelor_common.Json.get: couldn't find: " ^ path_to_string path)
  | Some value -> value

let get_or ~k ~default path json =
  match get_opt ~k path json with
  | None -> default
  | Some value -> value

let string = function
  | `String s -> Some s
  | _ -> None

let int = function
  | `Float f -> Some (int_of_float f)
  | _ -> None

let slug json =
  let open NesOption in
  string json >>= fun value ->
  Some (NesSlug.from_string value)

let rec list_map_opt (f : 'a -> 'b option) : 'a list -> 'b list option =
  let open NesOption in
  function
  | [] -> Some []
  | x :: l ->
    f x >>= fun x' ->
    list_map_opt f l >>= fun l' ->
    Some (x' :: l')

let strings = function
  | `A values -> list_map_opt string values
  | _ -> None

let list cast = function
  | `A values -> Some (List.map cast values)
  | _ -> None
