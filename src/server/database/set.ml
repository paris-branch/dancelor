open Nes
open Dancelor_common
open Model_new
open Search_new

module Entry_sql = Entry_sql.Sqlgg(Sqlgg_postgresql)
module Set_sql = Set_sql.Sqlgg(Sqlgg_postgresql)

type t = Model_builder.Core.Set.t
type entry = Model_builder.Core.Set.entry

let sql_to_row ~id ~name ~kind ~conceptors ~tunes ~permission ~(k : Set_row.t -> 'w) : 'w =
  k {
    id = Entry.Id.of_string_exn id;
    name;
    kind = Kind_dance.of_string kind;
    conceptors;
    tunes;
    permission = (match permission with `Everyone -> Everyone | `Owner -> Owner | `Viewer -> Viewer | `Omniscient_administrator -> Omniscient_administrator);
  }

let search ~user query : (Set_row.t * float) list Lwt.t =
  let {Query.common = {terms}; specific = ()} = query in
  Connection.with_ @@ fun db ->
  let%lwt tunes = Utils.fold_to_tbl Set_sql.Fold.get_all_tunes_new db (fun k ~set_id -> Version.sql_to_name ~k: (k set_id)) in
  let%lwt conceptors = Utils.fold_to_tbl Set_sql.Fold.get_all_conceptors_new db (fun k ~set_id -> Person.sql_to_name ~k: (k set_id)) in
  Set_sql.List.search
    db
    ~user_id: (Option.fold user ~some: Entry.Id.to_string ~none: "")
    ~terms
    (fun ~score ~id ->
      sql_to_row
        ~id
        ~tunes: (Utils.tbl_get tunes id)
        ~conceptors: (Utils.tbl_get conceptors id)
        ~k: (Pair.snoc score)
    )

let sql_to_set
    ~id
    ~name
    ~kind
    ~order
    ~remark
    ~created_at
    ~modified_at
    ~visibility
    ~conceptors
    ~content
    ~owners
    ~viewers
  =
  let visibility : Entry.Access.Private.visibility =
    match (visibility, viewers) with
    | (Some `Owners_only, []) -> Owners_only
    | (Some `Everyone, []) -> Everyone
    | (Some `Select_viewers, _) ->
      (
        match viewers with
        | [] -> assert false
        | _ -> Select_viewers (NEList.of_list_exn viewers)
      )
    | _ -> assert false
  in
  Entry.make
    ~id: (Entry.Id.of_string_exn id)
    ~meta: (Entry.Meta.make ~created_at ~modified_at ())
    ~access: (Entry.Access.Private.make ~owners: (NEList.of_list_exn owners) ~visibility ())
    (
      Model_builder.Core.Set.make
        ~name: (NEString.of_string_exn name)
        ~conceptors
        ~kind: (Kind_dance.of_string kind)
        ~contents: content
        ~order: (Model_builder.Core.Set_order.of_string order)
        ~remark: (Option.map NEString.of_string_exn remark)
        ()
    )

let set_to_sql ~create_or_update db id set =
  let id = Entry.Id.to_string id in
  ignore
  <$> create_or_update
      db
      ~id
      ~name: (NEString.to_string @@ Model_builder.Core.Set.name set)
      ~kind: (Kind_dance.to_string @@ Model_builder.Core.Set.kind set)
      ~order: (Model_builder.Core.Set_order.to_string @@ Model_builder.Core.Set.order set)
      ~remark: (Option.map NEString.to_string @@ Model_builder.Core.Set.remark set);%lwt
  ignore <$> Set_sql.delete_all_conceptors db ~set_id: id;%lwt
  Lwt_list.iter_s
    (fun conceptor ->
      ignore
      <$> Set_sql.add_one_conceptor
          db
          ~set_id: id
          ~conceptor_id: (Entry.Id.to_string conceptor)
    )
    (Model_builder.Core.Set.conceptors set);%lwt
  ignore <$> Set_sql.delete_all_content db ~set_id: id;%lwt
  Lwt_list.iteri_s
    (fun index (version, params) ->
      ignore
      <$> Set_sql.add_one_content_item
          db
          ~set_id: id
          ~index: (Int64.of_int index)
          ~version_id: (Entry.Id.to_string version)
          ~version_parameter_transposition_semitones: (Option.map (Int64.of_int % Transposition.to_semitones) @@ Model_builder.Core.Version_parameters.transposition params)
          ~version_parameter_first_bar: (Option.map Int64.of_int @@ Model_builder.Core.Version_parameters.first_bar params)
          ~version_parameter_clef: (Option.map Music.Clef.to_string @@ Model_builder.Core.Version_parameters.clef params)
          ~version_parameter_structure: (Option.map (NEString.to_string % Model_builder.Core.Version.Structure.to_string) @@ Model_builder.Core.Version_parameters.structure params)
          ~version_parameter_trivia: (Model_builder.Core.Version_parameters.trivia params)
          ~version_parameter_display_name: (Option.map NEString.to_string @@ Model_builder.Core.Version_parameters.display_name params)
          ~version_parameter_display_composer: (Option.map NEString.to_string @@ Model_builder.Core.Version_parameters.display_composer params)
    )
    (Model_builder.Core.Set.contents set)

let get id : Model_builder.Core.Set.entry option Lwt.t =
  let id = Entry.Id.to_string id in
  Connection.with_ @@ fun db ->
  let%lwt conceptors = Set_sql.List.get_conceptors db ~set_id: id (fun ~conceptor_id -> Entry.Id.of_string_exn conceptor_id) in
  let%lwt owners = Entry_sql.List.get_owners db ~entry_id: id (fun ~owner_id -> Entry.Id.of_string_exn owner_id) in
  let%lwt viewers = Entry_sql.List.get_viewers db ~entry_id: id (fun ~viewer_id -> Entry.Id.of_string_exn viewer_id) in
  let%lwt content =
    Set_sql.List.get_content db ~set_id: id (fun
        ~version_id
        ~version_parameter_transposition_semitones
        ~version_parameter_first_bar
        ~version_parameter_clef
        ~version_parameter_structure
        ~version_parameter_trivia
        ~version_parameter_display_name
        ~version_parameter_display_composer
      ->
      (
        Entry.Id.of_string_exn version_id,
        Model_builder.Core.Version_parameters.make
          ?transposition: (Option.map (Transposition.from_semitones % Int64.to_int) version_parameter_transposition_semitones)
          ?first_bar: (Option.map Int64.to_int version_parameter_first_bar)
          ?clef: (Option.map Music.Clef.of_string version_parameter_clef)
          ?structure: (Option.map (Option.get % Model_builder.Core.Version.Structure.of_string % NEString.of_string_exn) version_parameter_structure)
          ?trivia: version_parameter_trivia
          ?display_name: (Option.map NEString.of_string_exn version_parameter_display_name)
          ?display_composer: (Option.map NEString.of_string_exn version_parameter_display_composer)
          ()
      )
    )
  in
  Set_sql.Single.get db ~id (sql_to_set ~id ~conceptors ~viewers ~owners ~content)

let get_all () =
  Connection.with_ @@ fun db ->
  let conceptors = Hashtbl.create 8 in
  let owners = Hashtbl.create 8 in
  let viewers = Hashtbl.create 8 in
  let content = Hashtbl.create 8 in
  Set_sql.Fold.get_all_conceptors db (fun ~set_id ~conceptor_id () -> Hashtbl.add conceptors set_id @@ Entry.Id.of_string_exn conceptor_id) ();%lwt
  Entry_sql.Fold.get_all_owners db ~type_: `Set (fun ~entry_id ~owner_id () -> Hashtbl.add owners entry_id @@ Entry.Id.of_string_exn owner_id) ();%lwt
  Entry_sql.Fold.get_all_viewers db ~type_: `Set (fun ~entry_id ~viewer_id () -> Hashtbl.add viewers entry_id @@ Entry.Id.of_string_exn viewer_id) ();%lwt
  Set_sql.Fold.get_all_content
    db
    (fun
        ~set_id
        ~version_id
        ~version_parameter_transposition_semitones
        ~version_parameter_first_bar
        ~version_parameter_clef
        ~version_parameter_structure
        ~version_parameter_trivia
        ~version_parameter_display_name
        ~version_parameter_display_composer
        ()
      ->
      Hashtbl.add content set_id @@ (
        Entry.Id.of_string_exn version_id,
        Model_builder.Core.Version_parameters.make
          ?transposition: (Option.map (Transposition.from_semitones % Int64.to_int) version_parameter_transposition_semitones)
          ?first_bar: (Option.map Int64.to_int version_parameter_first_bar)
          ?clef: (Option.map Music.Clef.of_string version_parameter_clef)
          ?structure: (Option.map (Option.get % Model_builder.Core.Version.Structure.of_string % NEString.of_string_exn) version_parameter_structure)
          ?trivia: version_parameter_trivia
          ?display_name: (Option.map NEString.of_string_exn version_parameter_display_name)
          ?display_composer: (Option.map NEString.of_string_exn version_parameter_display_composer)
          ()
      )
    )
    ();%lwt
  Set_sql.List.get_all db (fun ~id ->
    sql_to_set
      ~id
      ~conceptors: (List.rev @@ Hashtbl.find_all conceptors id)
      ~viewers: (List.rev @@ Hashtbl.find_all viewers id)
      ~owners: (List.rev @@ Hashtbl.find_all owners id)
      ~content: (List.rev @@ Hashtbl.find_all content id)
  )

let create set access =
  Connection.with_ @@ fun db ->
  let%lwt id = Entry_new.make_private db `Set access in
  set_to_sql ~create_or_update: Set_sql.create db id set;%lwt
  lwt id

let update id set access =
  Connection.with_ @@ fun db ->
  Entry_new.touch db id;%lwt
  Entry_new.update_private_access db id access;%lwt
  set_to_sql ~create_or_update: (fun db ~id -> Set_sql.update db ~id) db id set

let delete id =
  Connection.with_ @@ fun db ->
  let set_id = Entry.Id.to_string id in
  ignore <$> Set_sql.delete_all_conceptors db ~set_id;%lwt
  ignore <$> Set_sql.delete_all_content db ~set_id;%lwt
  ignore <$> Set_sql.delete db ~id: set_id;%lwt
  Entry_new.delete db id
