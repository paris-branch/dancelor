open Nes
open Js_of_ocaml
open Dancelor_common
open Dancelor_client_model

let js = Js.string

type t = {
  mutable name : string;
  mutable alternative : string;
  mutable kind : string;
  mutable author : (Credit.t Slug.t * Credit.t) option;
  mutable scddb_id : string;
}

let create () =
{
  name = "";
  alternative = "";
  kind = "";
  author = None;
  scddb_id = "";
}

let name t =
  t.name

let set_name t name =
  t.name <- name

let alternative t =
  t.alternative

let set_alternative t name =
  t.alternative <- name

let kind t =
  t.kind

let set_kind t kind =
  t.kind <- kind

let author t =
  match t.author with
  | None -> None
  | Some (_, cr) -> Some cr

let set_author t slug =
  let%lwt author = Credit.get slug in
  t.author <- Some (slug, author);
  Lwt.return ()

let remove_author t =
  t.author <- None

let scddb_id t =
  t.scddb_id

let set_scddb_id t id =
  t.scddb_id <- id

let clear t =
  t.name <- "";
  t.alternative <- "";
  t.kind <- "";
  t.author <- None;
  t.scddb_id <- ""

let submit t =
  let name = t.name in
  let alternative_names = if t.alternative = "" then [] else [t.alternative] in
  let kind = Kind.base_of_string t.kind in
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
  Tune.make_and_save ~name ~alternative_names ~kind ?author:(author t) ?scddb_id ()
