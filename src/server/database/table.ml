open Nes
open Common

(** {2 Type of a model} *)

module type Model = sig
  type t

  val dependencies : t -> (string * unit Entry.Id.t) list

  val to_yojson : t -> Json.t
  val of_yojson : Json.t -> (t, string) result

  val wrap_any : t Entry.t -> ModelBuilder.Core.Any.t

  val _key : string
end

(** {2 Type of a table} *)

type 'value database_state = ('value Entry.Id.t, 'value) Hashtbl.t

type reverse_dependencies = ReverseDependencies of (string * unit Entry.Id.t) list
(** A type for reverse dependencies. *)

module type S = sig
  val _key : string

  type value

  type t = value database_state

  val load : unit -> unit Lwt.t
  val list_dependency_problems : unit -> Error.t list

  val get : value Entry.Id.t -> value Entry.t option
  val get_all : unit -> value Entry.t list

  val create : value -> value Entry.t Lwt.t
  (** Create a new database entry for the given value. *)

  val update : value Entry.Id.t -> value -> value Entry.t Lwt.t
  (** Update an existing database entry with the given value. *)

  val make_delete :
    (value Entry.Id.t -> reverse_dependencies) ->
    value Entry.Id.t ->
    unit Lwt.t
  (** Given a function that computes reverse dependencies, make a function that
      deletes an existing database entry if it is safe to do so. It throws
      {!Error.EntityHasReverseDependencies} otherwise. *)

  val dependencies : value -> unit Entry.Id.t list
  (** Pass {!Model.dependencies} through. *)

  module Log : Logs.LOG
end

(** {2 Global id uniqueness} *)

module GloballyUniqueId : sig
    val make : unit -> 'any Entry.Id.t
    (** Make a globally unique id. This does not register the id as already
        existing, and therefore there is a chance, by calling this function
        several times consecutively, that it returns the same id. *)

    val register :
      wrap_any: ('any Entry.t -> ModelBuilder.Core.Any.t) ->
      'any Entry.t ->
      unit
    (** Register an entry in the global id table. This is used to ensure that
        ids are globally unique across all tables. *)

    val get : 'any Entry.Id.t -> ModelBuilder.Core.Any.t option
    (** Given an id, try to find the corresponding model in the global table. *)
  end
= struct
  let all_ids_table : (unit Entry.Id.t, ModelBuilder.Core.Any.t) Hashtbl.t = Hashtbl.create 8

  let rec make () =
    let id = Entry.Id.make () in
    if not @@ Hashtbl.mem all_ids_table id then
      Entry.Id.unsafe_coerce id
    else
      make () (* extremely unlikely *)

  let register ~wrap_any entry =
    let id = Entry.Id.unsafe_coerce (Entry.id entry) in
    assert (not @@ Hashtbl.mem all_ids_table id);
    Hashtbl.add all_ids_table id (wrap_any entry)

  let get id = Hashtbl.find_opt all_ids_table @@ Entry.Id.unsafe_coerce id
end

(** {2 Database Functor} *)

module Make (Model : Model) : S with type value = Model.t = struct
  module Log = (val Logger.create ("database." ^ Model._key): Logs.LOG)

  let _key = Model._key

  type value = Model.t

  type t = value database_state

  let (table : (Model.t Entry.Id.t, Model.t Entry.t) Hashtbl.t) = Hashtbl.create 8

  let load () =
    Log.info (fun m -> m "Loading table: %s" _key);
    let load entry =
      Log.debug (fun m -> m "Loading %s %s" _key entry);
      Storage.read_entry_yaml Model._key entry "meta.yaml" >>= fun json ->
      lwt @@
        match Entry.of_yojson' (Entry.Id.of_string_exn entry) Model.of_yojson json with
        | Ok model ->
          GloballyUniqueId.register model ~wrap_any: Model.wrap_any;
          Hashtbl.add table (Entry.id model) model;
          Log.debug (fun m -> m "Loaded %s %s" _key entry);
        | Error msg ->
          Log.err (fun m -> m "Could not unserialize %s > %s > %s: %s" Model._key entry "meta.yaml" msg);
          exit 1
    in
    let%lwt entries = Storage.list_entries Model._key in
    Lwt_list.iter_s load entries;%lwt
    Log.info (fun m -> m "Loaded table: %s" _key);
    lwt_unit

  let get id = Hashtbl.find_opt table id

  let list_dependency_problems_for id status privacy dep_key dep_id =
    match GloballyUniqueId.get dep_id with
    | None ->
      [
        Error.dependency_does_not_exist
          ~source: (_key, Entry.Id.to_string id)
          ~dependency: (dep_key, Entry.Id.to_string dep_id)
      ]
    | Some dep_entry ->
      let dep_entry = ModelBuilder.Core.Any.to_entry dep_entry in
      let dep_meta = Entry.meta dep_entry in
      let dep_status = Entry.status dep_meta in
      let dep_privacy = Entry.privacy dep_meta in
      (
        if Status.can_depend status ~on: dep_status then []
        else
          [
            Error.dependency_violates_status
              ~source: (_key, Entry.Id.to_string id, status)
              ~dependency: (dep_key, Entry.Id.to_string dep_id, dep_status)
          ]
      ) @ (
        if Privacy.can_depend privacy ~on: dep_privacy then []
        else
          [
            Error.dependency_violates_privacy
              ~source: (_key, Entry.Id.to_string id, privacy)
              ~dependency: (dep_key, Entry.Id.to_string dep_id, dep_privacy)
          ]
      )

  let list_dependency_problems () =
    Hashtbl.to_seq_values table
    |> List.of_seq
    |> List.fold_left
        (fun problems model ->
          let id = Entry.id model in
          let status = Entry.(status % meta) model in
          let privacy = Entry.(privacy % meta) model in
          let deps = Model.dependencies @@ Entry.value model in
          let new_problems = List.map (uncurry @@ list_dependency_problems_for id status privacy) deps in
          new_problems
          |> List.flatten
          |> (fun new_problems -> new_problems @ problems)
        )
        []

  let get_all () = List.of_seq @@ Hashtbl.to_seq_values table

  let create_or_update maybe_id model =
    let (is_create, id, model) =
      match maybe_id with
      | None ->
        (* no id: this is a creation *)
        let id = GloballyUniqueId.make () in
        let model = Entry.make ~id model in
          (true, id, model)
      | Some id ->
        (* an id: this is an update *)
        let old_model = Option.get @@ get id in
        let model = Entry.make' ~id ~meta: (Entry.update_meta ~modified_at: (Datetime.now ()) (Entry.meta old_model)) model in
          (false, id, model)
    in
    let json = Entry.to_yojson' Model.to_yojson model in
    Storage.write_entry_yaml Model._key (Entry.Id.to_string id) "meta.yaml" json;%lwt
    Storage.save_changes_on_entry
      ~msg: (spf "%s %s / %s" (if is_create then "create" else "update") Model._key (Entry.Id.to_string id))
      Model._key
      (Entry.Id.to_string id);%lwt
    if is_create then
      (
        GloballyUniqueId.register model ~wrap_any: Model.wrap_any;
        Hashtbl.add table id model
      )
    else
      Hashtbl.replace table id model;
    lwt model

  let create = create_or_update None
  let update id model = create_or_update (Some id) model

  let dependencies = List.map snd % Model.dependencies

  let make_delete reverse_dependencies_of = fun id ->
    let rev_deps = reverse_dependencies_of id in
    match rev_deps with
    | ReverseDependencies [] ->
      Storage.delete_entry Model._key (Entry.Id.to_string id);%lwt
      Storage.save_changes_on_entry
        ~msg: (spf "delete %s / %s" Model._key (Entry.Id.to_string id))
        Model._key
        (Entry.Id.to_string id);%lwt
      Hashtbl.remove table id;
      lwt_unit
    | ReverseDependencies ((one_key, one_id) :: _) ->
      Log.warn (fun m ->
        m "Tried to remove %s / %s but it has reverse dependencies, for instance %s / %s" _key (Entry.Id.to_string id) one_key (Entry.Id.to_string one_id)
      );
      raise (Error.Exn (EntityHasReverseDependencies ()))
end
