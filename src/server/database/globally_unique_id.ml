open Nes
open Dancelor_common

module Globally_unique_id_sql = Globally_unique_id_sql.Sqlgg(Sqlgg_postgresql)

module Type = Model_builder.Core.Any.Type

let get db id =
  Option.map Type.of_string <$> Globally_unique_id_sql.get db ~id: (Entry.Id.to_string id)

let make db type_ =
  let rec make () =
    let id = Entry.Id.make () in
    match%lwt get db id with
    | None ->
      let%lwt _ = Globally_unique_id_sql.register db ~id: (Entry.Id.to_string id) ~type_: (Type.to_string type_) in
      lwt @@ Entry.Id.unsafe_coerce id
    | Some _ ->
      make () (* extremely unlikely *)
  in make ()
