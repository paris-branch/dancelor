open Nes
module Entry = Dancelor_common.Entry
module Status = Dancelor_common.Status

(** {2 Type of database statistics} *)

module Stats = struct
  type t = {mutable accesses: int}

  let empty () = {accesses = 0}

  let add_access stats =
    stats.accesses <- stats.accesses + 1

  let get_accesses stats =
    stats.accesses
end

(** {2 Type of a table} *)

type 'value database_state = ('value Slug.t, Stats.t * 'value) Hashtbl.t

module type S = sig
  val _key : string

  type value

  type t = value database_state

  val load : unit -> unit Lwt.t
  val list_dependency_problems : unit -> Dancelor_common.Error.t list Lwt.t
  val report_without_accesses : unit -> unit
  val standalone : bool

  val get : value Slug.t -> value Entry.t Lwt.t
  val get_opt : value Slug.t -> value Entry.t option Lwt.t
  val get_all : unit -> value Entry.t list Lwt.t

  val get_status : value Slug.t -> Status.t option Lwt.t

  val create : value -> value Entry.t Lwt.t
  (** Create a new database entry for the given value. *)

  val update : value Slug.t -> value -> value Entry.t Lwt.t
  (** Update an existing database entry with the given value. *)

  val save : ?slug: value Slug.t -> value -> value Entry.t Lwt.t
  (** If the slug is provided, then we {!update} an existing entry; otherwise,
      we {!create} a new one. *)

  val delete : value Slug.t -> unit Lwt.t

  module Log : Logs.LOG
end

(** *)

type 'value slug_and_table_unboxed = 'value Slug.t * (module S with type value = 'value)

type slug_and_table = Boxed : _ slug_and_table_unboxed -> slug_and_table

let make_slug_and_table (type value) (module Table : S with type value = value) (slug : value Slug.t) =
  Boxed (slug, (module Table: S with type value = value))

(** {2 Type of a model} *)

module type Model = sig
  type t

  val slug_hint : t -> string Lwt.t

  val dependencies : t Entry.t -> slug_and_table list Lwt.t

  val standalone : bool
  (** Whether entries of this table make sense on their own. *)

  val to_yojson : t -> Json.t
  val of_yojson : Json.t -> (t, string) result

  val separate_fields : (string * string) list
  (** Fields that must not be serialised in the [meta.yaml] file but instead be
      put in an independent file. The file name is given as second element of
      the pair. *)

  val _key : string
end

(** {2 Database Functor} *)

module Make (Model : Model) : S with type value = Model.t = struct
  module Log = (val Dancelor_server_logs.create ("database." ^ Model._key): Logs.LOG)

  let _key = Model._key
  let standalone = Model.standalone

  type value = Model.t

  type t = value database_state

  let (table : (Model.t Slug.t, (Stats.t * Model.t Entry.t)) Hashtbl.t) = Hashtbl.create 8

  let load () =
    Log.info (fun m -> m "Loading table: %s" _key);
    let load entry =
      Log.debug (fun m -> m "Loading %s %s" _key entry);
      Lwt.bind (Storage.read_entry_yaml Model._key entry "meta.yaml") @@ fun json ->
      let json = ref json in
      Lwt_list.iter_s
        (fun (field, filename) ->
           let%lwt value = Storage.read_entry_file Model._key entry filename in
           json := Json.add_field field (`String value) !json;
           Lwt.return_unit
        )
        Model.separate_fields;%lwt
      Lwt.return @@
      match Entry.of_yojson' (Slug.unsafe_of_string entry) Model.of_yojson !json with
      | Ok model ->
        Hashtbl.add table (Entry.slug model) (Stats.empty (), model);
        Log.debug (fun m -> m "Loaded %s %s" _key entry);
      | Error msg ->
        Log.err (fun m -> m "Could not unserialize %s > %s > %s: %s" Model._key entry "meta.yaml" msg);
        exit 1
    in
    let%lwt entries = Storage.list_entries Model._key in
    Lwt_list.iter_s load entries;%lwt
    Log.info (fun m -> m "Loaded table: %s" _key);
    Lwt.return_unit

  let get_opt slug =
    match Hashtbl.find_opt table slug with
    | Some (stats, model) ->
      Stats.add_access stats;
      Lwt.return_some model
    | None ->
      Lwt.return_none

  let get_status = Lwt.map (Option.map Entry.(status % meta)) % get_opt

  let list_dependency_problems_for slug status = function
    | Boxed (dep_slug, (module Dep_table)) ->
      match%lwt Dep_table.get_status dep_slug with
      | None ->
        [Dancelor_common.Error.DependencyDoesNotExist ((_key, Slug.to_string slug), (Dep_table._key, Slug.to_string dep_slug))]
        |> Lwt.return
      | Some dep_status ->
        if Status.ge dep_status status then
          Lwt.return_nil
        else
          [Dancelor_common.Error.DependencyViolatesStatus ((_key, Slug.to_string slug), (Dep_table._key, Slug.to_string dep_slug))]
          |> Lwt.return

  let list_dependency_problems () =
    Hashtbl.to_seq_values table
    |> List.of_seq
    |> Lwt_list.fold_left_s
      (fun problems (_, model) ->
         let slug = Entry.slug model in
         let status = Entry.(status % meta) model in
         let%lwt deps = Model.dependencies model in
         let%lwt new_problems =
           deps
           |> Lwt_list.map_s (list_dependency_problems_for slug status)
         in
         new_problems
         |> List.flatten
         |> (fun new_problems -> new_problems @ problems)
         |> Lwt.return
      )
      []

  let report_without_accesses () =
    Hashtbl.to_seq_values table
    |> Seq.iter
      (fun (stats, model) ->
         if Stats.get_accesses stats = 0 then
           Lwt.async (fun () ->
               let slug = Entry.slug model in
               Log.warn (fun m -> m "Without access: %s / %a" Model._key Slug.pp' slug);
               Lwt.return ()
             )
      )

  let get slug =
    match%lwt get_opt slug with
    | Some model -> Lwt.return model
    | None -> Lwt.fail Dancelor_common.Error.(Exn (EntityDoesNotExist (Model._key, Slug.to_string slug)))

  let get_all () =
    Hashtbl.to_seq_values table
    |> Seq.map
      (fun (stats, model) ->
         Stats.add_access stats;
         model
      )
    |> List.of_seq
    |> Lwt.return

  let uniq_slug ~hint : 'any Slug.t =
    let slug = Slug.from_string hint in
    let rec aux i =
      (* FIXME: cleaner way to do this *)
      let slug = Slug.(unsafe_of_string (to_string slug ^ "-" ^ (string_of_int i))) in
      if Hashtbl.mem table slug then
        aux (i + 1)
      else
        slug
    in
    if Hashtbl.mem table slug then
      aux 2
    else
      slug

  let create model =
    let%lwt slug_hint = Model.slug_hint model in
    let slug = uniq_slug ~hint: slug_hint in
    let model = Entry.make ~slug model in
    let json = ref @@ Entry.to_yojson' Model.to_yojson model in
    Lwt_list.iter_s
      (fun (field, filename) ->
         match Json.extract_field field !json with
         | (`String value, new_json) ->
           json := new_json;
           Storage.write_entry_file Model._key (Slug.to_string slug) filename value
         | _ -> assert false
      )
      Model.separate_fields;%lwt
    Storage.write_entry_yaml Model._key (Slug.to_string slug) "meta.yaml" !json;%lwt
    Storage.save_changes_on_entry
      ~msg: (spf "save %s / %s" Model._key (Slug.to_string slug))
      Model._key
      (Slug.to_string slug);%lwt
    Hashtbl.add table slug (Stats.empty (), model);
    (* FIXME: not add and not Stats.empty when editing. *)
    Lwt.return model

  let update slug model =
    let%lwt old_model = get slug in
    let model = Entry.make' ~slug ~meta: (Entry.update_meta ~modified_at: (Datetime.now ()) (Entry.meta old_model)) model in
    let json = ref @@ Entry.to_yojson' Model.to_yojson model in
    Lwt_list.iter_s
      (fun (field, filename) ->
         match Json.extract_field field !json with
         | (`String value, new_json) ->
           json := new_json;
           Storage.write_entry_file Model._key (Slug.to_string slug) filename value
         | _ -> assert false
      )
      Model.separate_fields;%lwt
    Storage.write_entry_yaml Model._key (Slug.to_string slug) "meta.yaml" !json;%lwt
    Storage.save_changes_on_entry
      ~msg: (spf "update %s / %s" Model._key (Slug.to_string slug))
      Model._key
      (Slug.to_string slug);%lwt
    (* FIXME: Make more robust and maybe update stats*)
    Hashtbl.replace table slug (fst (Hashtbl.find table slug), model);
    Lwt.return model

  let save ?slug model =
    match slug with
    | None -> create model
    | Some slug -> update slug model

  let delete slug : unit Lwt.t =
    Storage.delete_entry Model._key (Slug.to_string slug);%lwt
    Storage.save_changes_on_entry
      ~msg: (spf "delete %s / %s" Model._key (Slug.to_string slug))
      Model._key
      (Slug.to_string slug);%lwt
    Hashtbl.remove table slug;
    Lwt.return_unit
end
