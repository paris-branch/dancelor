open Nes
open Common

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

type 'value database_state = ('value Entry.Id.t, Stats.t * 'value) Hashtbl.t

module type S = sig
  val _key : string

  type value

  type t = value database_state

  val load : unit -> unit Lwt.t
  val list_dependency_problems : unit -> Error.t list Lwt.t
  val report_without_accesses : unit -> unit
  val standalone : bool

  val get : value Entry.Id.t -> value Entry.t Lwt.t
  val get_opt : value Entry.Id.t -> value Entry.t option Lwt.t
  val get_all : unit -> value Entry.t list Lwt.t

  val create : value -> value Entry.t Lwt.t
  (** Create a new database entry for the given value. *)

  val update : value Entry.Id.t -> value -> value Entry.t Lwt.t
  (** Update an existing database entry with the given value. *)

  val delete : value Entry.Id.t -> unit Lwt.t

  module Log : Logs.LOG
end

(** *)

type 'value id_and_table_unboxed = 'value Entry.Id.t * (module S with type value = 'value)

type id_and_table = Boxed : _ id_and_table_unboxed -> id_and_table

let make_id_and_table (type value) (module Table : S with type value = value) (id : value Entry.Id.t) =
  Boxed (id, (module Table: S with type value = value))

(** {2 Type of a model} *)

module type Model = sig
  type t

  val dependencies : t -> id_and_table list Lwt.t

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

(** {2 Global id uniqueness} *)

let (make_globally_unique_id, register_globally_unique_id) =
  let all_ids_table : (unit Entry.Id.t, unit) Hashtbl.t = Hashtbl.create 8 in
  let rec make_globally_unique_id () =
    let id = Entry.Id.make () in
    if not @@ Hashtbl.mem all_ids_table id then
      id
    else
      make_globally_unique_id () (* extremely unlikely *)
  in let register_globally_unique_id id =
    assert (not @@ Hashtbl.mem all_ids_table id);
    Hashtbl.add all_ids_table id ()
  in
    (make_globally_unique_id, register_globally_unique_id)

(** {2 Database Functor} *)

module Make (Model : Model) : S with type value = Model.t = struct
  module Log = (val Logger.create ("database." ^ Model._key): Logs.LOG)

  let _key = Model._key
  let standalone = Model.standalone

  type value = Model.t

  type t = value database_state

  let (table : (Model.t Entry.Id.t, (Stats.t * Model.t Entry.t)) Hashtbl.t) = Hashtbl.create 8

  let load () =
    Log.info (fun m -> m "Loading table: %s" _key);
    let load entry =
      Log.debug (fun m -> m "Loading %s %s" _key entry);
      Storage.read_entry_yaml Model._key entry "meta.yaml" >>= fun json ->
      let json = ref json in
      Lwt_list.iter_s
        (fun (field, filename) ->
          let%lwt value = Storage.read_entry_file Model._key entry filename in
          json := Json.add_field field (`String value) !json;
          lwt_unit
        )
        Model.separate_fields;%lwt
      lwt @@
        match Entry.of_yojson' (Entry.Id.of_string_exn entry) Model.of_yojson !json with
        | Ok model ->
          register_globally_unique_id (Entry.Id.unsafe_coerce @@ Entry.id model);
          Hashtbl.add table (Entry.id model) (Stats.empty (), model);
          Log.debug (fun m -> m "Loaded %s %s" _key entry);
        | Error msg ->
          Log.err (fun m -> m "Could not unserialize %s > %s > %s: %s" Model._key entry "meta.yaml" msg);
          exit 1
    in
    let%lwt entries = Storage.list_entries Model._key in
    Lwt_list.iter_s load entries;%lwt
    Log.info (fun m -> m "Loaded table: %s" _key);
    lwt_unit

  let get_opt id =
    match Hashtbl.find_opt table id with
    | Some (stats, model) ->
      Stats.add_access stats;
      lwt_some model
    | None ->
      lwt_none

  let list_dependency_problems_for id status privacy = function
    | Boxed (dep_id, (module Dep_table)) ->
      match%lwt Dep_table.get_opt dep_id with
      | None ->
        lwt [
          Error.dependency_does_not_exist
            ~source: (_key, Entry.Id.to_string id)
            ~dependency: (Dep_table._key, Entry.Id.to_string dep_id)
        ]
      | Some dep_entry ->
        let dep_meta = Entry.meta dep_entry in
        let dep_status = Entry.status dep_meta in
        let dep_privacy = Entry.privacy dep_meta in
        lwt @@
        (
          if Status.can_depend status ~on: dep_status then []
          else
            [
              Error.dependency_violates_status
                ~source: (_key, Entry.Id.to_string id, status)
                ~dependency: (Dep_table._key, Entry.Id.to_string dep_id, dep_status)
            ]
        ) @ (
          if Privacy.can_depend privacy ~on: dep_privacy then []
          else
            [
              Error.dependency_violates_privacy
                ~source: (_key, Entry.Id.to_string id, privacy)
                ~dependency: (Dep_table._key, Entry.Id.to_string dep_id, dep_privacy)
            ]
        )

  let list_dependency_problems () =
    Hashtbl.to_seq_values table
    |> List.of_seq
    |> Lwt_list.fold_left_s
        (fun problems (_, model) ->
          let id = Entry.id model in
          let status = Entry.(status % meta) model in
          let privacy = Entry.(privacy % meta) model in
          let%lwt deps = Model.dependencies @@ Entry.value model in
          let%lwt new_problems =
            deps
            |> Lwt_list.map_s (list_dependency_problems_for id status privacy)
          in
          new_problems
          |> List.flatten
          |> (fun new_problems -> new_problems @ problems)
          |> lwt
        )
        []

  let report_without_accesses () =
    Hashtbl.to_seq_values table
    |> Seq.iter
        (fun (stats, model) ->
          if Stats.get_accesses stats = 0 then
            Lwt.async (fun () ->
              let id = Entry.id model in
              Log.warn (fun m -> m "Without access: %s / %a" Model._key Entry.Id.pp' id);
              lwt_unit
            )
        )

  let get id =
    match%lwt get_opt id with
    | Some model -> lwt model
    | None -> Lwt.fail Error.(Exn (EntityDoesNotExist (Model._key, Entry.Id.to_string id)))

  let get_all () =
    Hashtbl.to_seq_values table
    |> Seq.map
        (fun (stats, model) ->
          Stats.add_access stats;
          model
        )
    |> List.of_seq
    |> lwt

  let create model =
    let id = Entry.Id.unsafe_coerce @@ make_globally_unique_id () in
    let model = Entry.make ~id model in
    let json = ref @@ Entry.to_yojson' Model.to_yojson model in
    Lwt_list.iter_s
      (fun (field, filename) ->
        match Json.extract_field field !json with
        | (`String value, new_json) ->
          json := new_json;
          Storage.write_entry_file Model._key (Entry.Id.to_string id) filename value
        | _ -> assert false
      )
      Model.separate_fields;%lwt
    Storage.write_entry_yaml Model._key (Entry.Id.to_string id) "meta.yaml" !json;%lwt
    Storage.save_changes_on_entry
      ~msg: (spf "save %s / %s" Model._key (Entry.Id.to_string id))
      Model._key
      (Entry.Id.to_string id);%lwt
    register_globally_unique_id (Entry.Id.unsafe_coerce @@ Entry.id model);
    Hashtbl.add table id (Stats.empty (), model);
    (* FIXME: not add and not Stats.empty when editing. *)
    lwt model

  let update id model =
    let%lwt old_model = get id in
    let model = Entry.make' ~id ~meta: (Entry.update_meta ~modified_at: (Datetime.now ()) (Entry.meta old_model)) model in
    let json = ref @@ Entry.to_yojson' Model.to_yojson model in
    Lwt_list.iter_s
      (fun (field, filename) ->
        match Json.extract_field field !json with
        | (`String value, new_json) ->
          json := new_json;
          Storage.write_entry_file Model._key (Entry.Id.to_string id) filename value
        | _ -> assert false
      )
      Model.separate_fields;%lwt
    Storage.write_entry_yaml Model._key (Entry.Id.to_string id) "meta.yaml" !json;%lwt
    Storage.save_changes_on_entry
      ~msg: (spf "update %s / %s" Model._key (Entry.Id.to_string id))
      Model._key
      (Entry.Id.to_string id);%lwt
    (* FIXME: Make more robust and maybe update stats*)
    Hashtbl.replace table id (fst (Hashtbl.find table id), model);
    lwt model

  let delete id : unit Lwt.t =
    Storage.delete_entry Model._key (Entry.Id.to_string id);%lwt
    Storage.save_changes_on_entry
      ~msg: (spf "delete %s / %s" Model._key (Entry.Id.to_string id))
      Model._key
      (Entry.Id.to_string id);%lwt
    Hashtbl.remove table id;
    lwt_unit
end
