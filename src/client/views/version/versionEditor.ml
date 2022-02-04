open Js_of_ocaml

let js = Js.string

type t = {
  mutable name : string;
}

let create () = {
  name = ""
}

let name t =
  t.name

let set_name t name =
  t.name <- name

let clear t =
  t.name <- ""

let submit _t =
  (* TODO : Requires endpoints to save a version *)
  failwith "TODO"
