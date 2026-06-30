open Nes
open Dancelor_common
open Model_new
open Search_new
open Sql_to_row
open Sql_to_view

module Person_sql = Person_sql.Sqlgg(Sqlgg_postgresql)

let get_row id : Person_row.t option Lwt.t =
  Connection.with_ @@ fun db ->
  Person_sql.Single.get_row db ~id (person_sql_to_row ~id ~k: Fun.id)

let get_rows ids : (Person_id.t, Person_row.t) Utils.tbl Lwt.t =
  Connection.with_ @@ fun db ->
  Utils.fold_to_tbl (Person_sql.Fold.get_rows ~ids) db (fun k ~id -> person_sql_to_row ~id ~k: (k id))

let get_view id : Person_view.t option Lwt.t =
  Connection.with_ @@ fun db ->
  Person_sql.Single.get_view db ~id (person_sql_to_view ~id ~k: Fun.id)

let get_row_for_user (id : User_id.t) : Person_row.t option Lwt.t =
  Connection.with_ @@ fun db ->
  Person_sql.Single.get_row_for_user db ~id (person_sql_to_row ~k: Fun.id)

let search query : (Person_row.t * float) list Lwt.t =
  let {Query.common = {terms}; specific = ()} = query in
  Connection.with_ @@ fun db ->
  Person_sql.List.search
    db
    ~terms
    (fun ~score -> person_sql_to_row ~k: (Pair.snoc score))

(* Legacy *)

type t = Model_builder.Core.Person.t
type entry = Model_builder.Core.Person.entry

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
    ~id
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
    ~id
    ~name: (NEString.to_string @@ Model_builder.Core.Person.name person)
    ~scddb_id: (Option.map Int64.of_int @@ Model_builder.Core.Person.scddb_id person)
    ~composed_tunes_are_public: (Model_builder.Core.Person.composed_tunes_are_public person)
    ~published_tunes_are_public: (Model_builder.Core.Person.published_tunes_are_public person)

let get id : Model_builder.Core.Person.entry option Lwt.t =
  Connection.with_ @@ fun db ->
  Person_sql.Single.get db ~id (sql_to_person ~id)

let create person =
  Connection.with_ @@ fun db ->
  let%lwt id = Entry_new.make_public db `Person in
  let%lwt _ = person_to_sql ~create_or_update: (Person_sql.create db) id person in
  lwt id

let update id person =
  Connection.with_ @@ fun db ->
  Entry_new.touch db id;%lwt
  ignore <$> person_to_sql ~create_or_update: (fun ~id -> Person_sql.update db ~id) id person

let delete id =
  Connection.with_ @@ fun db ->
  ignore <$> Person_sql.delete db ~id;%lwt
  Entry_new.delete db id
