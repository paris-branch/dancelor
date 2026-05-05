open Nes
open Dancelor_common

module Person_sql = Person_sql.Sqlgg(Sqlgg_postgresql)

type t = Model_builder.Core.Person.t
type entry = Model_builder.Core.Person.entry

let row_to_person
    ~id
    ~name
    ~scddb_id
    ~composed_tunes_are_public
    ~published_tunes_are_public
    ~created_at
    ~modified_at
  =
  Entry.make
    ~id: (Entry.Id.of_string_exn id)
    ~meta: (Entry.Meta.make ~created_at ~modified_at ())
    ~access: Entry.Access.Public
    (
      Model_builder.Core.Person.make
        ~name: (NEString.of_string_exn name)
        ~scddb_id: (Option.map Int64.to_int scddb_id)
        ~composed_tunes_are_public
        ~published_tunes_are_public
        ()
    )

let get id : Model_builder.Core.Person.entry option Lwt.t =
  let id = Entry.Id.to_string id in
  Connection.with_ @@ fun db ->
  Person_sql.Single.get db ~id (row_to_person ~id)

let get_all () =
  Connection.with_ @@ fun db ->
  Person_sql.List.get_all db row_to_person

let create person =
  let%lwt id = Globally_unique_id.make Person in
  Connection.with_ @@ fun db ->
  let%lwt _ =
    Person_sql.create
      db
      ~id: (Entry.Id.to_string id)
      ~name: (NEString.to_string @@ Model_builder.Core.Person.name person)
      ~scddb_id: (Option.map Int64.of_int @@ Model_builder.Core.Person.scddb_id person)
      ~composed_tunes_are_public: (Model_builder.Core.Person.composed_tunes_are_public person)
      ~published_tunes_are_public: (Model_builder.Core.Person.published_tunes_are_public person)
  in
  lwt id

let update id person =
  Connection.with_ @@ fun db ->
  ignore
  <$> Person_sql.update
      db
      ~id: (Entry.Id.to_string id)
      ~name: (NEString.to_string @@ Model_builder.Core.Person.name person)
      ~scddb_id: (Option.map Int64.of_int @@ Model_builder.Core.Person.scddb_id person)
      ~composed_tunes_are_public: (Model_builder.Core.Person.composed_tunes_are_public person)
      ~published_tunes_are_public: (Model_builder.Core.Person.published_tunes_are_public person)

let delete id =
  let%lwt _ =
    Connection.with_ @@ fun db ->
    Person_sql.delete db ~id: (Entry.Id.to_string id)
  in
  lwt_unit
