open Nes
open Dancelor_common

module Dance_sql = Dance_sql.Sqlgg(Sqlgg_postgresql)

type t = Model_builder.Core.Dance.t
type entry = Model_builder.Core.Dance.entry

(* NOTE: Do not reorder as that would break serialisation to and deserialisation
   from PostgreSQL. *)
(* FIXME: We should just have a proper enum in PostgreSQL... *)
type two_chords =
  | Dont_know
  | One_chord
  | Two_chords
[@@deriving enum]

let two_chords_to_common : two_chords -> Model_builder.Core.Dance.two_chords = function
  | Dont_know -> Dont_know
  | One_chord -> One_chord
  | Two_chords -> Two_chords

let two_chords_of_common : Model_builder.Core.Dance.two_chords -> two_chords = function
  | Dont_know -> Dont_know
  | One_chord -> One_chord
  | Two_chords -> Two_chords

let row_to_dance
    ~id
    ~name
    ~extra_names
    ~kind
    ~two_chords
    ~scddb_id
    ~disambiguation
    ~date
    ~created_at
    ~modified_at
    ~devisers
  =
  Entry.make
    ~id: (Entry.Id.of_string_exn id)
    ~meta: (Entry.Meta.make ~created_at ~modified_at ())
    ~access: Entry.Access.Public
    (
      Model_builder.Core.Dance.make
        ~names: (NEList.cons (NEString.of_string_exn name) extra_names)
        ~kind: (Kind_dance.of_string kind)
        ~two_chords: (two_chords_to_common @@ Option.get @@ two_chords_of_enum @@ Int64.to_int two_chords)
        ~scddb_id: (Option.map Int64.to_int scddb_id)
        ~disambiguation: (Option.map NEString.of_string_exn disambiguation)
        ~date: (Option.map (Option.get % PartialDate.from_string) date)
        ~devisers
        ()
    )

let dance_to_row ~create_or_update db id dance =
  (* FIXME: transaction, maybe [Connection.with_transaction] *)
  let id = Entry.Id.to_string id in
  ignore
  <$> create_or_update
      db
      ~id
      ~name: (NEString.to_string @@ NEList.hd @@ Model_builder.Core.Dance.names dance)
      ~kind: (Kind_dance.to_string @@ Model_builder.Core.Dance.kind dance)
      ~two_chords: (Int64.of_int @@ two_chords_to_enum @@ two_chords_of_common @@ Model_builder.Core.Dance.two_chords dance)
      ~scddb_id: (Option.map Int64.of_int @@ Model_builder.Core.Dance.scddb_id dance)
      ~disambiguation: (Option.map NEString.to_string @@ Model_builder.Core.Dance.disambiguation dance)
      ~date: (Option.map PartialDate.to_string @@ Model_builder.Core.Dance.date dance);%lwt
  ignore <$> Dance_sql.delete_all_extra_names db ~dance_id: id;%lwt
  Lwt_list.iter_s
    (fun extra_name ->
      ignore <$> Dance_sql.add_one_extra_name db ~dance_id: id ~extra_name: (NEString.to_string extra_name)
    )
    (NEList.tl @@ Model_builder.Core.Dance.names dance);%lwt
  ignore <$> Dance_sql.delete_all_devisers db ~dance_id: id;%lwt
  Lwt_list.iteri_s
    (fun index deviser_id ->
      ignore
      <$> Dance_sql.add_one_deviser
          db
          ~dance_id: id
          ~index: (Int64.of_int index)
          ~deviser_id: (Entry.Id.to_string deviser_id)
    )
    (Model_builder.Core.Dance.devisers dance)

let get id : Model_builder.Core.Dance.entry option Lwt.t =
  let id = Entry.Id.to_string id in
  Connection.with_ @@ fun db ->
  let%lwt extra_names = Dance_sql.List.get_extra_names db ~dance_id: id (fun ~extra_name -> NEString.of_string_exn extra_name) in
  let%lwt devisers = Dance_sql.List.get_devisers db ~dance_id: id (fun ~deviser_id -> Entry.Id.of_string_exn deviser_id) in
  Dance_sql.Single.get db ~id (row_to_dance ~id ~extra_names ~devisers)

let get_all () =
  Connection.with_ @@ fun db ->
  let extra_names = Hashtbl.create 8 in
  let devisers = Hashtbl.create 8 in
  Dance_sql.Fold.get_all_extra_names
    db
    (fun ~dance_id ~extra_name () ->
      Hashtbl.add extra_names dance_id (NEString.of_string_exn extra_name)
    )
    ();%lwt
  Dance_sql.Fold.get_all_devisers
    db
    (fun ~dance_id ~deviser_id () ->
      Hashtbl.add devisers dance_id (Entry.Id.of_string_exn deviser_id)
    )
    ();%lwt
  Dance_sql.List.get_all db (fun ~id ->
    row_to_dance
      ~id
      ~extra_names: (List.rev @@ Hashtbl.find_all extra_names id)
      ~devisers: (List.rev @@ Hashtbl.find_all devisers id)
  )

let create dance =
  Connection.with_ @@ fun db ->
  let%lwt id = Globally_unique_id.make db Dance in
  dance_to_row ~create_or_update: Dance_sql.create db id dance;%lwt
  lwt id

let update id dance =
  Connection.with_ @@ fun db ->
  dance_to_row ~create_or_update: (fun db ~id -> Dance_sql.update db ~id) db id dance

let delete id =
  Connection.with_ @@ fun db ->
  let dance_id = Entry.Id.to_string id in
  ignore <$> Dance_sql.delete_all_extra_names db ~dance_id;%lwt
  ignore <$> Dance_sql.delete_all_devisers db ~dance_id;%lwt
  ignore <$> Dance_sql.delete db ~id: dance_id
