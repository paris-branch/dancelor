open Nes
open Dancelor_common
open Dancelor_client_model

type t = {
  mutable name : string;
  mutable count : int;
  mutable scddb_id : string;
}

let create () =
  {
    name = "";
    count = 0;
    scddb_id = "";
  }

let name t =
  t.name

let set_name t name =
  t.name <- name

let count t =
  t.count

let scddb_id t =
  t.scddb_id

let set_scddb_id t id =
  t.scddb_id <- id

let clear t =
  t.name <- "";
  t.count <- 0;
  t.scddb_id <- ""

let submit t =
  (* The fact that the string is an integer will have been checked in the form *)
  let scddb_id =
    if t.scddb_id = "" then
      None
    else
      match int_of_string_opt t.scddb_id with
      | Some scddb_id -> Some scddb_id
      | None ->
        match SCDDB.person_from_uri t.scddb_id with
        | Ok scddb_id -> Some scddb_id
        | Error _ -> None
  in
  let modified_at = Datetime.now () in
  let created_at = Datetime.now () in
  Credit.make_and_save ~line:t.name ?scddb_id ~modified_at ~created_at ()
