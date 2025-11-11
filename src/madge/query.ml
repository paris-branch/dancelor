open Nes

type key = string
type t = (key * Yojson.Safe.t) list

let empty = []

let is_empty = (=) []

let get = List.assoc_opt
let extract = List.extract_assoc_opt

let singleton key value = [key, value]

let to_list = Fun.id

let to_strings =
  List.map (fun (key, value) -> (key, [Yojson.Safe.to_string value]))

let from_uri uri =
  try
    some @@
      List.map
        (fun (k, vs) ->
          (
            k,
            match vs with
            | [v] -> Yojson.Safe.from_string v
            | vs -> `List (List.map Yojson.Safe.from_string vs)
          )
        )
        (Uri.query uri)
  with
    | Yojson.Json_error _ -> None

let from_body body =
  let body = if body = "" then "{}" else body in
  match Yojson.Safe.from_string body with
  | `Assoc body -> body
  | _ -> assert false

let add k v = List.cons (k, v)
