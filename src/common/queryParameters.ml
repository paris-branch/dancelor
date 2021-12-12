type key = string
type t = (key * Yojson.Safe.t) list

let get = List.assoc_opt
let get_exn = List.assoc

let to_list = Fun.id

let from_uri uri =
  List.map
    (fun (k, vs) ->
       (k,
        match vs with
        | [v] -> Yojson.Safe.from_string v
        | vs -> `List (List.map Yojson.Safe.from_string vs)))
    (Uri.query uri)

let from_body body =
  let%lwt body = Cohttp_lwt.Body.to_string body in
  let body = if body = "" then "{}" else body in
  let body = Yojson.Safe.from_string body in
  let body = match body with `Assoc body -> body | _ -> assert false in
  Lwt.return body

let append ~high ~low =
  high @ low (** FIXME: remove duplicates *)
