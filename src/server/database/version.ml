open Nes
open Dancelor_common

module Version_sql = Version_sql.Sqlgg(Sqlgg_postgresql)

type t = Model_builder.Core.Version.t
type entry = Model_builder.Core.Version.entry

let row_to_version
    ~id
    ~tune_id
    ~key
    ~remark
    ~disambiguation
    ~monolithic_lilypond
    ~monolithic_bars
    ~monolithic_or_default_structure
    ~created_at
    ~modified_at
    ~arrangers
    ~sources
    ~destructured_parts
    ~destructured_transitions
  =
  let content : Model_builder.Core.Version.Content.t =
    match (monolithic_lilypond, monolithic_bars),
    (destructured_parts, destructured_transitions),
    monolithic_or_default_structure with
    | (None, None), ([], []), None ->
      No_content
    | (Some lilypond, Some bars), ([], []), Some structure ->
      Monolithic {lilypond; bars = Int64.to_int bars; structure = Option.get (Model_builder.Core.Version.Structure.of_string (NEString.of_string_exn structure))}
    | (None, None), (parts, transitions), Some default_structure ->
      (
        match NEList.of_list parts with
        | None -> assert false
        | Some parts -> Destructured {parts; transitions; default_structure = Option.get (Model_builder.Core.Version.Structure.of_string (NEString.of_string_exn default_structure))}
      )
    | _ -> assert false
  in
  Entry.make
    ~id: (Entry.Id.of_string_exn id)
    ~meta: (Entry.Meta.make ~created_at ~modified_at ())
    ~access: Entry.Access.Public
    (
      Model_builder.Core.Version.make
        ~tune: (Entry.Id.of_string_exn tune_id)
        ~key: (Music.Key.of_string key)
        ~remark: (Option.map NEString.of_string_exn remark)
        ~disambiguation: (Option.map NEString.of_string_exn disambiguation)
        ~sources
        ~arrangers
        ~content
        ()
    )

let version_to_row ~create_or_update db id version =
  (* FIXME: transaction, maybe [Connection.with_transaction] *)
  let (monolithic_lilypond, monolithic_bars, monolithic_or_default_structure) =
    match Model_builder.Core.Version.content version with
    | No_content -> (None, None, None)
    | Monolithic {lilypond; bars; structure} -> (Some lilypond, Some (Int64.of_int bars), Some (NEString.to_string @@ Model_builder.Core.Version.Structure.to_string structure))
    | Destructured {default_structure; _} -> (None, None, Some (NEString.to_string @@ Model_builder.Core.Version.Structure.to_string default_structure))
  in
  let id = Entry.Id.to_string id in
  ignore
  <$> create_or_update
      db
      ~id
      ~tune_id: (Entry.Id.to_string @@ Model_builder.Core.Version.tune version)
      ~key: (Music.Key.to_string @@ Model_builder.Core.Version.key version)
      ~remark: (Option.map NEString.to_string @@ Model_builder.Core.Version.remark version)
      ~disambiguation: (Option.map NEString.to_string @@ Model_builder.Core.Version.disambiguation version)
      ~monolithic_lilypond
      ~monolithic_bars
      ~monolithic_or_default_structure;%lwt
  ignore <$> Version_sql.delete_all_arrangers db ~version_id: id;%lwt
  Lwt_list.iter_s
    (fun arranger ->
      ignore
      <$> Version_sql.add_one_arranger
          db
          ~version_id: id
          ~arranger_id: (Entry.Id.to_string arranger)
    )
    (Model_builder.Core.Version.arrangers version);%lwt
  ignore <$> Version_sql.delete_all_sources db ~version_id: id;%lwt
  Lwt_list.iter_s
    (fun Model_builder.Core.Version.{source; structure; details} ->
      ignore
      <$> Version_sql.add_one_source
          db
          ~version_id: id
          ~source_id: (Entry.Id.to_string source)
          ~structure: (NEString.to_string @@ Model_builder.Core.Version.Structure.to_string structure)
          ~details: (Option.map NEString.to_string details)
    )
    (Model_builder.Core.Version.sources version);%lwt
  (
    ignore <$> Version_sql.delete_all_destructured_parts db ~version_id: id;%lwt
    ignore <$> Version_sql.delete_all_destructured_transitions db ~version_id: id;%lwt
    match Model_builder.Core.Version.content version with
    | No_content | Monolithic _ -> lwt_unit
    | Destructured {parts; transitions; default_structure = _} ->
      Lwt_list.iteri_s
        (fun part Model_builder.Core.Version.Voices.{melody; chords} ->
          ignore
          <$> Version_sql.add_one_destructured_part
              db
              ~version_id: id
              ~part: Model_builder.Core.Version.Part_name.(to_string @@ of_int part)
              ~melody
              ~chords
        )
        (NEList.to_list parts);%lwt
      Lwt_list.iter_s
        (fun (from_parts, to_parts, Model_builder.Core.Version.Voices.{melody; chords}) ->
          ignore
          <$> Version_sql.add_one_destructured_transition
              db
              ~version_id: id
              ~from_parts: (Model_builder.Core.Version.Part_name.opens_to_string from_parts)
              ~to_parts: (Model_builder.Core.Version.Part_name.opens_to_string to_parts)
              ~melody
              ~chords
        )
        transitions
  )

let check_destructured_parts =
  (* NOTE: A bit weird: on the OCaml side, part names are implicit and just come
     from the order in the list, while in SQL we store the part name/number. SQL
     sorts for us, but now we need to check that they correspond. *)
  List.mapi (fun i (part, voices) ->
    if Model_builder.Core.Version.Part_name.to_int part <> i then assert false
    else voices
  )

let get id : Model_builder.Core.Version.entry option Lwt.t =
  let id = Entry.Id.to_string id in
  Connection.with_ @@ fun db ->
  let%lwt arrangers = Version_sql.List.get_arrangers db ~version_id: id (fun ~arranger_id -> Entry.Id.of_string_exn arranger_id) in
  let%lwt sources =
    Version_sql.List.get_sources db ~version_id: id (fun ~source_id ~structure ~details ->
      {
        Model_builder.Core.Version.source = Entry.Id.of_string_exn source_id;
        structure = Option.get (Model_builder.Core.Version.Structure.of_string (NEString.of_string_exn structure));
        details = Option.map NEString.of_string_exn details;
      }
    )
  in
  let%lwt destructured_parts =
    check_destructured_parts
    <$> (
        Version_sql.List.get_destructured_parts db ~version_id: id (fun ~part ~melody ~chords ->
          (
            Option.get (Model_builder.Core.Version.Part_name.of_string part),
            {Model_builder.Core.Version.Voices.melody; chords}
          )
        )
      )
  in
  let%lwt destructured_transitions =
    Version_sql.List.get_destructured_transitions db ~version_id: id (fun ~from_parts ~to_parts ~melody ~chords ->
      (
        Option.get (Model_builder.Core.Version.Part_name.opens_of_string from_parts),
        Option.get (Model_builder.Core.Version.Part_name.opens_of_string to_parts),
        {Model_builder.Core.Version.Voices.melody; chords}
      )
    )
  in
  Version_sql.Single.get db ~id (row_to_version ~id ~arrangers ~sources ~destructured_parts ~destructured_transitions)

let get_all () =
  Connection.with_ @@ fun db ->
  let arrangers = Hashtbl.create 8 in
  let sources = Hashtbl.create 8 in
  let destructured_parts = Hashtbl.create 8 in
  let destructured_transitions = Hashtbl.create 8 in
  Version_sql.Fold.get_all_arrangers
    db
    (fun ~version_id ~arranger_id () ->
      Hashtbl.add arrangers version_id (Entry.Id.of_string_exn arranger_id)
    )
    ();%lwt
  Version_sql.Fold.get_all_sources
    db
    (fun ~version_id ~source_id ~structure ~details () ->
      Hashtbl.add
        sources
        version_id
        {
          Model_builder.Core.Version.source = Entry.Id.of_string_exn source_id;
          structure = Option.get (Model_builder.Core.Version.Structure.of_string (NEString.of_string_exn structure));
          details = Option.map NEString.of_string_exn details;
        }
    )
    ();%lwt
  Version_sql.Fold.get_all_destructured_parts
    db
    (fun ~version_id ~part ~melody ~chords () ->
      Hashtbl.add destructured_parts version_id (
        Option.get (Model_builder.Core.Version.Part_name.of_string part),
        {Model_builder.Core.Version.Voices.melody; chords}
      )
    )
    ();%lwt
  Version_sql.Fold.get_all_destructured_transitions
    db
    (fun ~version_id ~from_parts ~to_parts ~melody ~chords () ->
      Hashtbl.add
        destructured_transitions
        version_id
        (
          Option.get (Model_builder.Core.Version.Part_name.opens_of_string from_parts),
          Option.get (Model_builder.Core.Version.Part_name.opens_of_string to_parts),
          {Model_builder.Core.Version.Voices.melody; chords}
        )
    )
    ();%lwt
  Version_sql.List.get_all db (fun ~id ->
    row_to_version
      ~id
      ~arrangers: (List.rev @@ Hashtbl.find_all arrangers id)
      ~sources: (List.rev @@ Hashtbl.find_all sources id)
      ~destructured_parts: (check_destructured_parts @@ List.rev @@ Hashtbl.find_all destructured_parts id)
      ~destructured_transitions: (List.rev @@ Hashtbl.find_all destructured_transitions id)
  )

let get_all_for_tune tune_id =
  let tune_id = Entry.Id.to_string tune_id in
  Connection.with_ @@ fun db ->
  let arrangers = Hashtbl.create 8 in
  let sources = Hashtbl.create 8 in
  let destructured_parts = Hashtbl.create 8 in
  let destructured_transitions = Hashtbl.create 8 in
  Version_sql.Fold.get_all_arrangers
    db
    (fun ~version_id ~arranger_id () ->
      Hashtbl.add arrangers version_id (Entry.Id.of_string_exn arranger_id)
    )
    ();%lwt
  Version_sql.Fold.get_all_sources
    db
    (fun ~version_id ~source_id ~structure ~details () ->
      Hashtbl.add
        sources
        version_id
        {
          Model_builder.Core.Version.source = Entry.Id.of_string_exn source_id;
          structure = Option.get (Model_builder.Core.Version.Structure.of_string (NEString.of_string_exn structure));
          details = Option.map NEString.of_string_exn details;
        }
    )
    ();%lwt
  Version_sql.Fold.get_all_destructured_parts
    db
    (fun ~version_id ~part ~melody ~chords () ->
      Hashtbl.add destructured_parts version_id (
        Option.get (Model_builder.Core.Version.Part_name.of_string part),
        {Model_builder.Core.Version.Voices.melody; chords}
      )
    )
    ();%lwt
  Version_sql.Fold.get_all_destructured_transitions
    db
    (fun ~version_id ~from_parts ~to_parts ~melody ~chords () ->
      Hashtbl.add
        destructured_transitions
        version_id
        (
          Option.get (Model_builder.Core.Version.Part_name.opens_of_string from_parts),
          Option.get (Model_builder.Core.Version.Part_name.opens_of_string to_parts),
          {Model_builder.Core.Version.Voices.melody; chords}
        )
    )
    ();%lwt
  Version_sql.List.get_all_for_tune db ~tune_id (fun ~id ->
    row_to_version
      ~id
      ~tune_id
      ~arrangers: (List.rev @@ Hashtbl.find_all arrangers id)
      ~sources: (List.rev @@ Hashtbl.find_all sources id)
      ~destructured_parts: (check_destructured_parts @@ List.rev @@ Hashtbl.find_all destructured_parts id)
      ~destructured_transitions: (List.rev @@ Hashtbl.find_all destructured_transitions id)
  )

let create version =
  Connection.with_ @@ fun db ->
  let%lwt id = Globally_unique_id.make db Version in
  version_to_row ~create_or_update: Version_sql.create db id version;%lwt
  lwt id

let update id version =
  Connection.with_ @@ fun db ->
  version_to_row ~create_or_update: (fun db ~id -> Version_sql.update db ~id) db id version

let delete id =
  Connection.with_ @@ fun db ->
  let version_id = Entry.Id.to_string id in
  ignore <$> Version_sql.delete_all_arrangers db ~version_id;%lwt
  ignore <$> Version_sql.delete_all_sources db ~version_id;%lwt
  ignore <$> Version_sql.delete_all_destructured_parts db ~version_id;%lwt
  ignore <$> Version_sql.delete_all_destructured_transitions db ~version_id;%lwt
  ignore <$> Version_sql.delete db ~id: version_id
