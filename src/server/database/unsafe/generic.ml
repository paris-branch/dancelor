open Nes

module type Model = sig
  type t
  val slug : t -> t Slug.t Lwt.t
  val status : t -> Dancelor_common_model.Status.t Lwt.t

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

module Make (Model : Model) = struct
  module Log = (val Dancelor_server_logs.create ("database.unsafe." ^ Model._key) : Logs.LOG)

  type t = (Model.t Slug.t, Stats.t * Model.t) Hashtbl.t
  let create () : t = Hashtbl.create 8

  let read_separated_file model file =
    let%lwt slug = Model.slug model in
    Lwt.return (Storage.read_entry_file Model._key slug file)

  let write_separated_file model file content =
    let%lwt slug = Model.slug model in
    Storage.write_entry_file Model._key slug file content;
    Lwt.return ()

  let initialise () =
    let database = create () in
    let load entry =
      let json = Storage.read_entry_json Model._key entry "meta.json" in
      let json = Json.add_field "slug" (`String entry) json in
      match Model.of_yojson json with
      | Ok model ->
        let%lwt slug = Model.slug model in
        Hashtbl.add database slug (Stats.empty (), model);
        Lwt.return ()
      | Error msg ->
        Log.err (fun m -> m "Could not unserialize %s > %s > %s: %s" Model._key entry "meta.json" msg);
        exit 1
    in
    let%lwt () = Lwt_list.iter_s load (Storage.list_entries Model._key) in
    Lwt.return database

  let report_without_accesses ~database =
    Hashtbl.to_seq_values database
    |> Seq.iter
      (fun (stats, model) ->
         if Stats.get_accesses stats = 0 then
           Log.warn (fun m -> Lwt.async (fun () ->
               let%lwt slug = Model.slug model in
               m "Without access: %s / %s" Model._key slug;
               Lwt.return ()))) (* FIXME *)

  let uniq_slug ~database ~hint =
    let slug = Slug.from_string hint in
    let rec aux i =
      let slug = slug ^ "-" ^ (string_of_int i) in
      if Hashtbl.mem database slug then
        aux (i+1)
      else
        slug
    in
    if Hashtbl.mem database slug then
      aux 2
    else
      slug

  let get ~database slug =
    match Hashtbl.find_opt database slug with
    | Some (stats, model) ->
      Stats.add_access stats;
      Lwt.return model
    | None ->
      Lwt.fail Dancelor_common.Error.(Exn (EntityDoesNotExist (Model._key, slug)))

  let get_all ~database =
    Hashtbl.to_seq_values database
    |> Seq.map
      (fun (stats, model) ->
         Stats.add_access stats;
         model)
    |> List.of_seq
    |> Lwt.return

  let is_status_ge ~database ~status slug =
    let%lwt model_status =
      try%lwt
        get ~database slug >>=| Model.status
      with
        Dancelor_common.Error.Exn _ as exn ->
        Log.err (fun m -> m "Not found: %s" slug);
        Lwt.fail exn
    in
    Lwt.return (Dancelor_common_model.Status.ge model_status status)

  module Static = struct
    (* Here comes the static world. If we are happy with a database, we apply it
       and it becomes static. The other functions can then use it. *)

    let database = ref (create ())
    let establish = (:=) database

    let get slug = get ~database:!database slug
    let get_all () = get_all ~database:!database

    (* Save and delete have side effects; they belong always to the static world. *)

    let save ~slug_hint with_slug =
      let slug = uniq_slug ~database:!database ~hint:slug_hint in
      let%lwt model = with_slug slug in
      let json = Model.to_yojson model in
      let json = Json.remove_field "slug" json in
      Storage.write_entry_json Model._key slug "meta.json" json;
      Storage.save_changes_on_entry
        ~msg:(spf "[auto] save %s / %s" Model._key slug)
        Model._key slug;
      Hashtbl.add !database slug (Stats.empty (), model); (* FIXME: not add and not Stats.empty when editing. *)
      Lwt.return model

    let delete slug =
      Storage.delete_entry Model._key slug;
      Storage.save_changes_on_entry
        ~msg:(spf "[auto] delete %s / %s" Model._key slug)
        Model._key slug;
      Hashtbl.remove !database slug;
      Lwt.return ()
  end
end
