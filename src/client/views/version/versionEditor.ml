open Nes
open Js_of_ocaml
open Dancelor_client_model

let js = Js.string

type t = {
  mutable tune : (Tune.t Slug.t * Tune.t) option;
  mutable bars : string;
  mutable key : string;
  mutable structure : string;
  mutable remark : string;
  mutable content : string;
}

let create () = {
  tune = None;
  bars = "";
  key = "";
  structure = "";
  remark = "";
  content = ""
}

let tune t =
  match t.tune with
  | None -> None
  | Some (_, tune) -> Some tune

let set_tune t slug =
  let%lwt tune = Tune.get slug in
  t.tune <- Some (slug, tune);
  Lwt.return ()

let remove_tune t =
  t.tune <- None

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

let remark t =
  t.remark

let set_remark t r =
  t.remark <- r

let content t =
  t.content

let set_content t content =
  t.content <- content

let clear t =
  t.tune <- None;
  t.bars <- "";
  t.key <- "";
  t.structure <- "";
  t.remark <- "";
  t.content <- ""

let submit t =
  let tune = match tune t with
  | None -> failwith "Empty tune"
  | Some tune -> tune
  in
  let bars = int_of_string t.bars in
  let key = Music.key_of_string t.key in
  let structure = t.structure in
  let remark = if t.remark = "" then None else Some t.remark in
  let content = t.content in
  Version.make_and_save ~tune ~bars ~key ~structure ~content ?remark ()
