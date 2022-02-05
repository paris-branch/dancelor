open Nes
open Js_of_ocaml
open Dancelor_client_model

let js = Js.string

type t = {
  mutable tune : (Tune.t Slug.t * Tune.t) option;
  mutable bars : string;
  mutable key : string;
  mutable structure : string;
}

let create () = {
  tune = None;
  bars = "";
  key = "";
  structure = ""
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

let clear t =
  t.tune <- None;
  t.bars <- "";
  t.key <- "";
  t.structure <- ""

let submit _t =
  (* TODO : Requires endpoints to save a version *)
  failwith "TODO"
