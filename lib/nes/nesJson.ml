open NesOption.Syntax

type t = Yojson.Safe.t

let add_field key value = function
  | `Assoc fields when not (List.mem_assoc key fields) ->
    `Assoc ((key, value) :: fields)
  | _ ->
    failwith "NesJson.add_field"

let add_fields fields json =
  List.fold_left
    (fun json (field, value) ->
       add_field field value json)
    json
    fields

let map_field key fun_ = function
  | `Assoc fields when List.mem_assoc key fields ->
    `Assoc
      (List.map
         (fun (key', value) ->
            (key',
             if key = key'
             then fun_ value
             else value))
         fields)
  | _ -> failwith "NesJson.map_field"

let remove_field key = function
  | `Assoc fields when List.mem_assoc key fields ->
    `Assoc (List.remove_assoc key fields)
  | _ ->
    failwith "NesJson.remove_field"

let from_string str = Yojson.Safe.from_string str
let to_string json = Yojson.Safe.pretty_to_string ~std:true json

let rec find_opt path json =
  match path, json with
  | [], _ -> Some json
  | key :: path, `Assoc fields when List.mem_assoc key fields ->
    find_opt path (List.assoc key fields)
  | _, _ -> None

let find path json =
  match find_opt path json with
  | None -> failwith ("NesJson.find: could not find: " ^ String.concat " > " path)
  | Some json -> json

let get_opt ~k path json =
  find_opt path json >>=? fun value ->
  k value

let get ~k path json =
  match get_opt ~k path json with
  | None -> failwith ("NesJson.get: could not find: " ^ String.concat " > " path)
  | Some value -> value

let get_or ~k ~default path json =
  match get_opt ~k path json with
  | None -> default
  | Some value -> value

let string = function
  | `String s -> Some s
  | _ -> None

let int = function
  | `Int i -> Some i
  | _ -> None

let slug json =
  string json >>=? fun value ->
  Some (NesSlug.from_string value)

let rec list_map_opt (f : 'a -> 'b option) : 'a list -> 'b list option =
  function
  | [] -> Some []
  | x :: l ->
    f x >>=? fun x' ->
    list_map_opt f l >>=? fun l' ->
    Some (x' :: l')

let strings = function
  | `List values -> list_map_opt string values
  | _ -> None

let list cast = function
  | `List values -> Some (List.map cast values)
  | _ -> None
