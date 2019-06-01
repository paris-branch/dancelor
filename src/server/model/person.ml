include Dancelor_common_model.Person

(* * *)

let get = Dancelor_server_database.Person.get

let make_and_save ~name () =
  Dancelor_server_database.Person.save ~slug_hint:name @@ fun slug ->
  unsafe_make ~slug ~name ()
