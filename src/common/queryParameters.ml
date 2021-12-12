type key = string
type t = (key * Yojson.Safe.t) list

let get = List.assoc_opt
let get_exn = List.assoc

let to_list = Fun.id
let from_list = Fun.id
