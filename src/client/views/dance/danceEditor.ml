open Nes
open Dancelor_common
open Dancelor_client_model

type t = {
  mutable name : string;
  mutable kind : string;
  mutable date : string;
  mutable deviser : (Person.t Slug.t * Person.t) option;
  mutable two_chords : bool;
  mutable scddb_id : string;
}

let create () =
  {
    name = "";
    kind = "";
    date = "";
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

let date t =
  t.date

let set_date t date =
  t.date <- date

let deviser t =
  match t.deviser with
  | None -> None
  | Some (_, cr) -> Some cr

let set_deviser t slug =
  let%lwt deviser = Person.get slug in
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
  t.date <- "";
  t.deviser <- None;
  t.two_chords <- false;
  t.scddb_id <- ""

let submit t =
  let name = t.name in
  let kind = Kind.Dance.of_string t.kind in
  let date = if t.date = "" then None else Some (PartialDate.from_string t.date) in
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
  let modified_at = Datetime.now () in
  let created_at = Datetime.now () in
  Dance.make_and_save ~name ~kind ?devisers:(Option.map List.singleton (deviser t))
    ~two_chords ?scddb_id ?date ~modified_at ~created_at ()
