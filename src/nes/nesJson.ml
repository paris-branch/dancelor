type t = Yojson.Safe.t

let add_field key value = function
  | `Assoc fields when not (List.mem_assoc key fields) ->
    `Assoc ((key, value) :: fields)
  | _ ->
    failwith "NesJson.add_field"

let extract_field_opt key = function
  | `Assoc fields ->
    (
      match NesList.extract_assoc_opt key fields with
      | None -> None
      | Some (value, fields) -> Some (value, `Assoc fields)
    )
  | _ -> invalid_arg "NesJson.extract_field_opt: not an `Assoc"

let extract_field key json =
  match extract_field_opt key json with
  | None -> failwith @@ "extract_field: no such field: " ^ key
  | Some (value, json) -> (value, json)

let from_string str = Yojson.Safe.from_string str

let rec find_opt path json =
  match path, json with
  | [], _ -> Some json
  | key :: path, `Assoc fields when List.mem_assoc key fields ->
    find_opt path (List.assoc key fields)
  | _, _ -> None

let get_opt ~k path json =
  Option.bind (find_opt path json) k

let string = function
  | `String s -> Some s
  | _ -> None

let int = function
  | `Int i -> Some i
  | _ -> None

let merge_assoc j1 j2 =
  match (j1, j2) with
  | `Assoc a1, `Assoc a2 -> `Assoc (a1 @ a2)
  | _ -> invalid_arg "NesJson.merge_assoc"

let keep_fields ks = function
  | `Assoc l ->
    `Assoc (
      List.map
        (fun k ->
          match List.assoc_opt k l with
          | None -> failwith "NesJson.keep_fields"
          | Some x -> (k, x)
        )
        ks
    )
  | _ -> invalid_arg "NesJson.keep_fields"
