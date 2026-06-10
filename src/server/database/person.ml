open Nes
open Dancelor_common
open Model_new

module Person_sql = Person_sql.Sqlgg(Sqlgg_postgresql)

type t = Model_builder.Core.Person.t
type entry = Model_builder.Core.Person.entry

let sql_to_name ~id ~name ~(k : Person_name.t -> 'w) : 'w =
  k {id = Entry.Id.of_string_exn id; name}

let sql_to_row ~id ~name (k : Person_row.t -> 'w) : 'w =
  k {id = Entry.Id.of_string_exn id; name}

let search needle : (Person_row.t * float) list Lwt.t =
  Connection.with_ @@ fun db ->
  Person_sql.List.search
    db
    ~needle
    (fun ~score -> sql_to_row (Pair.snoc score))

let sql_to_person
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

let person_to_sql ~create_or_update id person =
  create_or_update
    ~id: (Entry.Id.to_string id)
    ~name: (NEString.to_string @@ Model_builder.Core.Person.name person)
    ~scddb_id: (Option.map Int64.of_int @@ Model_builder.Core.Person.scddb_id person)
    ~composed_tunes_are_public: (Model_builder.Core.Person.composed_tunes_are_public person)
    ~published_tunes_are_public: (Model_builder.Core.Person.published_tunes_are_public person)

let get id : Model_builder.Core.Person.entry option Lwt.t =
  let id = Entry.Id.to_string id in
  Connection.with_ @@ fun db ->
  Person_sql.Single.get db ~id (sql_to_person ~id)

let get_all () =
  Connection.with_ @@ fun db ->
  Person_sql.List.get_all db sql_to_person

let create person =
  Connection.with_ @@ fun db ->
  let%lwt id = Entry_new.make db `Person in
  let%lwt _ = person_to_sql ~create_or_update: (Person_sql.create db) id person in
  lwt id

let update id person =
  Connection.with_ @@ fun db ->
  ignore <$> person_to_sql ~create_or_update: (fun ~id -> Person_sql.update db ~id) id person

let delete id =
  let%lwt _ =
    Connection.with_ @@ fun db ->
    Person_sql.delete db ~id: (Entry.Id.to_string id)
  in
  lwt_unit
