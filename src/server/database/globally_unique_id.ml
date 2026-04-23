open Nes
open Dancelor_common

module Globally_unique_id_sql = Globally_unique_id_sql.Sqlgg(Connection.Sqlgg_mariadb_lwt)

let get db id =
  Globally_unique_id_sql.Fold.get
    db
    ~id: (Entry.Id.to_string id)
    (fun ~type_ _ -> Some (Model_builder.Core.Any.Type.of_string type_))
    None

let rec make type_ =
  (* FIXME: Can be done in an atomic way with an operation that fails, catching
     the error. *)
  Connection.with_ @@ fun db ->
  let id = Entry.Id.make () in
  match%lwt get db id with
  | None ->
    let%lwt _ : int64 =
      Globally_unique_id_sql.register
        db
        ~id: (Entry.Id.to_string id)
        ~type_: (Model_builder.Core.Any.Type.to_string type_)
    in
    lwt @@ Entry.Id.unsafe_coerce id
  | Some _ ->
    make type_ (* extremely unlikely *)

let get id = Connection.with_ @@ fun db -> get db id
