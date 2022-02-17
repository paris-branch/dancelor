open Nes
open Dancelor_common
open Dancelor_client_model

type t = {
  mutable name : string;
  mutable kind : string;
  mutable deviser : (Credit.t Slug.t * Credit.t) option;
  mutable two_chords : bool;
  mutable scddb_id : string;
}

let create () =
{
  name = "";
  kind = "";
  deviser = None;
  two_chords = false;
  scddb_id = ""
}

let name t =
  t.name

let set_name t name =
  t.name <- name

let kind t =
  t.kind

let set_kind t kind =
  t.kind <- kind

let deviser t =
  match t.deviser with
  | None -> None
  | Some (_, cr) -> Some cr

let set_deviser t slug =
  let%lwt deviser = Credit.get slug in
  t.deviser <- Some (slug, deviser);
  Lwt.return ()

let remove_deviser t =
  t.deviser <- None

let two_chords t =
  t.two_chords

let set_two_chords t tc =
  t.two_chords <- tc

let scddb_id t =
  t.scddb_id

let set_scddb_id t id =
  t.scddb_id <- id

let clear t =
  t.name <- "";
  t.kind <- "";
  t.deviser <- None;
  t.two_chords <- false;
  t.scddb_id <- ""

let submit t =
  let name = t.name in
  let kind = Kind.base_of_string t.kind in
  let two_chords = t.two_chords in
  let scddb_id =
    if t.scddb_id = "" then
      None
    else
      match int_of_string_opt t.scddb_id with
      | Some scddb_id -> Some scddb_id
      | None ->
        match SCDDB.tune_from_uri t.scddb_id with
        | Ok scddb_id -> Some scddb_id
        | Error _ -> None
  in
  Dance.make_and_save ~name ~kind ?deviser:(deviser t) ~two_chords ?scddb_id ()


