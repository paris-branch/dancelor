type key = string
type t = (key * Yojson.Safe.t ) list

let get = List.assoc_opt

exception WrongType of string * string
let wrong_type expected provided =
  let provided =
    match provided with
    | `Null -> "null"
    | `Bool _ -> "bool"
    | `Int _ -> "int"
    | `Intlit _ -> "intlit"
    | `Float _ -> "float"
    | `String _ -> "string"
    | `Assoc _ -> "assoc"
    | `List _ -> "list"
    | `Tuple _ -> "tuple"
    | `Variant _ -> "variant"
  in
  raise (WrongType (expected, provided))

let get_string k p =
  match%opt get k p with
  | `String s -> Some s
  | j -> wrong_type "string" j

let to_list = Fun.id

let from_uri uri =
  List.map
    (fun (k, vs) ->
      (
        k,
        match vs with
        | [v] -> Yojson.Safe.from_string v
        | vs -> `List (List.map Yojson.Safe.from_string vs)
      ))
    (Uri.query uri)

let from_body body =
  let%lwt body = Cohttp_lwt.Body.to_string body in
  let body = if body = "" then "{}" else body in
  let body = Yojson.Safe.from_string body in
  let body = match body with `Assoc body -> body | _ -> assert false in
  Lwt.return body

let append ~high ~low =
  high @ low (** FIXME: remove duplicates *)
