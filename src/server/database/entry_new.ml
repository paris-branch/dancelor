open Nes
open Dancelor_common

module Entry_sql = Entry_sql.Sqlgg(Sqlgg_postgresql)

type type_ = [
  | `Book
  | `Dance
  | `Person
  | `Set
  | `Source
  | `Tune
  | `User
  | `Version
]

let get db id =
  Entry_sql.get db ~id: (Entry.Id.to_string id)

let make db type_ =
  let rec make () =
    let id = Entry.Id.make () in
    match%lwt get db id with
    | None ->
      let%lwt _ = Entry_sql.register db ~id: (Entry.Id.to_string id) ~type_ in
      lwt @@ Entry.Id.unsafe_coerce id
    | Some _ ->
      make () (* extremely unlikely *)
  in make ()

let touch db id =
  ignore <$> Entry_sql.touch db ~id: (Entry.Id.to_string id)

let delete _db _id =
  (* FIXME: delete? soft delete? *)
  (* NOTE: Keeping all the entries because we have more than enough
     ids, and this way we avoid reusing the same one after another one
     was deleted, and in doing so making links exist again. *)
  lwt_unit
