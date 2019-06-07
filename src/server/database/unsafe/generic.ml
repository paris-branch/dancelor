open Nes
module Storage = Dancelor_server_database_storage

module type Model = sig
  type t
  val slug : t -> t Slug.t Lwt.t

  val to_yojson : t -> Json.t
  val of_yojson : Json.t -> (t, string) result

  val _key : string
end

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

module Make (Log : Logs.LOG) (Model : Model) = struct
  let db : (Model.t Slug.t, Stats.t * Model.t) Hashtbl.t = Hashtbl.create 8

  let read_separated_file slug file =
    Storage.read_entry_file Model._key slug file

  let initialise () =
    let load entry =
      let json = Storage.read_entry_json Model._key entry "meta.json" in
      let json = Json.add_field "slug" (`String entry) json in
      match Model.of_yojson json with
      | Ok model ->
        let%lwt slug = Model.slug model in
        Hashtbl.add db slug (Stats.empty (), model);
        Lwt.return ()
      | Error msg ->
        Log.err (fun m -> m "Could not unserialize %s > %s > %s: %s" Model._key entry "meta.json" msg);
        exit 1
    in
    Storage.list_entries Model._key
    |> Lwt_list.iter_s load

  let report_without_accesses () =
    Hashtbl.to_seq_values db
    |> Seq.iter
      (fun (stats, model) ->
         if Stats.get_accesses stats = 0 then
           Log.warn (fun m -> Lwt.async (fun () ->
               let%lwt slug = Model.slug model in
               m "Without access: %s / %s" Model._key slug;
               Lwt.return ()))) (* FIXME *)

  let uniq_slug ~hint =
    let slug = Slug.from_string hint in
    let rec aux i =
      let slug = slug ^ "-" ^ (string_of_int i) in
      if Hashtbl.mem db slug then
        aux (i+1)
      else
        slug
    in
    if Hashtbl.mem db slug then
      aux 2
    else
      slug

  let get slug =
    match Hashtbl.find_opt db slug with
    | Some (stats, model) ->
      Stats.add_access stats;
      Lwt.return model
    | None ->
      Lwt.fail Dancelor_common.Error.(Exn (EntityDoesNotExist (Model._key, slug)))

  let get_all () =
    Hashtbl.to_seq_values db
    |> Seq.map
      (fun (stats, model) ->
         Stats.add_access stats;
         model)
    |> List.of_seq
    |> Lwt.return

  let save ~slug_hint with_slug =
    let slug = uniq_slug ~hint:slug_hint in
    let%lwt model = with_slug slug in
    let json = Model.to_yojson model in
    let json = Json.remove_field "slug" json in
    Storage.write_entry_json Model._key slug "meta.json" json;
    Storage.save_changes_on_entry
      ~msg:(spf "[auto] save %s / %s" Model._key slug)
      Model._key slug;
    Hashtbl.add db slug (Stats.empty (), model); (* FIXME: not add and not Stats.empty when editing. *)
    Lwt.return model

  let delete slug =
    Storage.delete_entry Model._key slug;
    Storage.save_changes_on_entry
      ~msg:(spf "[auto] delete %s / %s" Model._key slug)
      Model._key slug;
    Hashtbl.remove db slug;
    Lwt.return ()
end
