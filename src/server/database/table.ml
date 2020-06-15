open Nes

(** {2 Type of database statistics} *)

module Stats = struct
  type t =
    { mutable accesses : int }

  let empty () =
    { accesses = 0 }

  let add_access stats =
    stats.accesses <- stats.accesses + 1

  let get_accesses stats =
    stats.accesses
end

(** {2 Version of a database} *)

module Version = struct
  type t = int

  let flag = ref 0
  let create () = incr flag; !flag

  let equal = (=)
end

(** {2 Type of a table} *)

type 'value database_state = ('value Slug.t, Stats.t * 'value) Hashtbl.t

module type S = sig
  val _key : string

  type value

  type t = value database_state

  val create_version : version:Version.t -> unit
  val delete_version : version:Version.t -> unit

  val load_version : version:Version.t -> unit Lwt.t
  val list_dependency_problems : version:Version.t -> Dancelor_common.Error.t list Lwt.t
  val report_without_accesses : version:Version.t -> unit (* FIXME *)

  val establish_version : version:Version.t -> unit

  (* The establish_version introduces the notion of a default version of the
     table. The following functions can either specify a specific version or use
     the default one. *)

  val get : ?version:Version.t -> value Slug.t -> value Lwt.t
  val get_opt : ?version:Version.t -> value Slug.t -> value option Lwt.t
  val get_all : ?version:Version.t -> unit -> value list Lwt.t

  val get_status : ?version:Version.t -> value Slug.t -> Dancelor_common_model.Status.t option Lwt.t

  (* The next functions only work for the default version as they include
     writing on the disk. *)

  val save : slug_hint:string -> (value Slug.t -> value Lwt.t) -> value Lwt.t
  val delete : value Slug.t -> unit Lwt.t

  val read_separated_file : value -> string -> string Lwt.t
  val write_separated_file : value -> string -> string -> unit Lwt.t

  (* Logging *)

  module Log : Logs.LOG
end

(** *)

type 'value slug_and_table_unboxed = 'value Slug.t * (module S with type value = 'value)

type slug_and_table = Boxed : _ slug_and_table_unboxed -> slug_and_table

let make_slug_and_table (type value) (module Table : S with type value = value) (slug : value Slug.t) =
  Boxed (slug, (module Table : S with type value = value))

(** {2 Type of a model} *)

module type Model = sig
  type t
  val slug : t -> t Slug.t Lwt.t
  val status : t -> Dancelor_common_model.Status.t Lwt.t

  val dependencies : t -> slug_and_table list Lwt.t

  val to_yojson : t -> Json.t
  val of_yojson : Json.t -> (t, string) result

  val _key : string
end

(** {2 Database Functor} *)

module Make (Model : Model) : S with type value = Model.t = struct
  module Log = (val Dancelor_server_logs.create ("database." ^ Model._key) : Logs.LOG)

  let _key = Model._key

  type value = Model.t

  type t = value database_state

  let versions : (Version.t, t) Hashtbl.t = Hashtbl.create 2

  let create_version ~version =
    if Hashtbl.mem versions version then
      failwith "cannot create existing version";
    Hashtbl.add versions version (Hashtbl.create 8)

  let default_version = ref None
  let default = ref None

  let establish_version ~version =
    default_version := Some version;
    default := Some (Hashtbl.find versions version)

  let delete_version ~version =
    let is_default =
      match !default_version with
      | None -> false
      | Some default_version -> Version.equal version default_version
    in
    if is_default then
      failwith "cannot delete default table";
    Hashtbl.remove versions version

  let get_table ?version () =
    match version with
    | None ->
      (
        match !default with
        | None -> failwith "no default table"
        | Some default -> default
      )
    | Some version ->
      Hashtbl.find versions version

  let load_version ~version =
    let table = get_table ~version () in
    let load entry =
      let json = Storage.read_entry_json Model._key entry "meta.json" in
      let json = Json.add_field "slug" (`String entry) json in
      match Model.of_yojson json with
      | Ok model ->
        let%lwt slug = Model.slug model in
        Hashtbl.add table slug (Stats.empty (), model);
        Lwt.return ()
      | Error msg ->
        Log.err (fun m -> m "Could not unserialize %s > %s > %s: %s" Model._key entry "meta.json" msg);
        exit 1
    in
    Lwt_list.iter_s load (Storage.list_entries Model._key)

  let get_opt ?version slug =
    let table = get_table ?version () in
    match Hashtbl.find_opt table slug with
    | Some (stats, model) ->
      Stats.add_access stats;
      Lwt.return_some model
    | None ->
      Lwt.return_none

  let get_status ?version slug =
    match%lwt get_opt ?version slug with
    | None ->
      Lwt.return_none
    | Some model ->
      let%lwt status = Model.status model in
      Lwt.return_some status

  let list_dependency_problems_for slug status ~version = function
    | Boxed (dep_slug, (module Dep_table)) ->
      match%lwt Dep_table.get_status ~version dep_slug with
      | None ->
        [Dancelor_common.Error.DependencyDoesNotExist((_key, slug), (Dep_table._key, dep_slug))]
        |> Lwt.return
      | Some dep_status ->
        if Dancelor_common_model.Status.ge dep_status status then
          Lwt.return_nil
        else
          [Dancelor_common.Error.DependencyViolatesStatus((_key, slug), (Dep_table._key, dep_slug))]
          |> Lwt.return

  let list_dependency_problems ~version =
    get_table ~version ()
    |> Hashtbl.to_seq_values
    |> List.of_seq
    |> Lwt_list.fold_left_s
      (fun problems (_, model) ->
         let%lwt slug = Model.slug model in
         let%lwt status = Model.status model in
         let%lwt deps = Model.dependencies model in
         let%lwt new_problems =
           deps
           |> Lwt_list.map_s (list_dependency_problems_for slug status ~version)
         in
         new_problems
         |> List.flatten
         |> (fun new_problems -> new_problems @ problems)
         |> Lwt.return)
      []

  let report_without_accesses ~version =
    get_table ~version ()
    |> Hashtbl.to_seq_values
    |> Seq.iter
      (fun (stats, model) ->
         if Stats.get_accesses stats = 0 then
           Log.warn (fun m -> Lwt.async (fun () ->
               let%lwt slug = Model.slug model in
               m "Without access: %s / %s" Model._key slug;
               Lwt.return ()))) (* FIXME *)

  let get ?version slug =
    match%lwt get_opt ?version slug with
    | Some model ->
      Lwt.return model
    | None ->
      Lwt.fail Dancelor_common.Error.(Exn (EntityDoesNotExist (Model._key, slug)))

  let get_all ?version () =
    get_table ?version ()
    |> Hashtbl.to_seq_values
    |> Seq.map
      (fun (stats, model) ->
         Stats.add_access stats;
         model)
    |> List.of_seq
    |> Lwt.return

  (* World of Side Effects; no versions here. *)

  let uniq_slug ~hint =
    let table = get_table () in
    let slug = Slug.from_string hint in
    let rec aux i =
      let slug = slug ^ "-" ^ (string_of_int i) in
      if Hashtbl.mem table slug then
        aux (i+1)
      else
        slug
    in
    if Hashtbl.mem table slug then
      aux 2
    else
      slug

  let save ~slug_hint with_slug =
    let table = get_table () in
    let slug = uniq_slug ~hint:slug_hint in
    let%lwt model = with_slug slug in
    let json = Model.to_yojson model in
    let json = Json.remove_field "slug" json in
    Storage.write_entry_json Model._key slug "meta.json" json;
    let%lwt () =
      Storage.save_changes_on_entry
        ~msg:(spf "[auto] save %s / %s" Model._key slug)
        Model._key slug
    in
    Hashtbl.add table slug (Stats.empty (), model); (* FIXME: not add and not Stats.empty when editing. *)
    Lwt.return model

  let delete slug =
    let table = get_table () in
    Storage.delete_entry Model._key slug;
    let%lwt () =
      Storage.save_changes_on_entry
        ~msg:(spf "[auto] delete %s / %s" Model._key slug)
        Model._key slug
    in
    Hashtbl.remove table slug;
    Lwt.return ()

  let read_separated_file model file =
    let%lwt slug = Model.slug model in
    Lwt.return (Storage.read_entry_file Model._key slug file)

  let write_separated_file model file content =
    let%lwt slug = Model.slug model in
    Storage.write_entry_file Model._key slug file content;
    Lwt.return ()
end