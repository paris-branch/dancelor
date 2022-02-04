open Js_of_ocaml

let js = Js.string

type t = {
  mutable name : string;
  mutable bars : string;
  mutable key : string;
  mutable structure : string;
}

let create () = {
  name = "";
  bars = "";
  key = "";
  structure = ""
}

let name t =
  t.name

let set_name t name =
  t.name <- name

let bars t =
  t.bars

let set_bars t bars =
  t.bars <- bars

let key t =
  t.key

let set_key t key =
  t.key <- key

let structure t =
  t.structure

let set_structure t s =
  t.structure <- s

let clear t =
  t.name <- "";
  t.bars <- "";
  t.key <- "";
  t.structure <- ""

let submit _t =
  (* TODO : Requires endpoints to save a version *)
  failwith "TODO"
