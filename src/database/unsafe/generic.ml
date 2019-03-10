open Dancelor_common open Option

module type Model = sig
  type t
  val slug : t -> t Slug.t
  val serialize : t -> Json.t
  val unserialize : Json.t -> t

  val prefix : string
  val separated_files : string list
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

  let initialise () =
    let load entry =
      let json = Storage.read_entry_json Model.prefix entry "meta.json" in
      let json = Json.add_field "slug" (`String entry) json in
      let json =
        List.fold_left
          (fun json file ->
             Json.add_field
               (Filename.chop_extension file)
               (`String (Storage.read_entry_file Model.prefix entry file))
               json)
          json
          Model.separated_files
      in
      let model = Model.unserialize json in
      Hashtbl.add db (Model.slug model) (Stats.empty (), model)
    in
    Storage.list_entries Model.prefix
    |> List.iter load

  let report_without_accesses () =
    Hashtbl.to_seq_values db
    |> Seq.iter
      (fun (stats, model) ->
         if Stats.get_accesses stats = 0 then
           Log.warn (fun m -> m "Without access: %s / %s"
                        Model.prefix (Model.slug model)))

  let find_uniq_slug string =
    let slug = Slug.from_string string in
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
    let (stats, model) = Hashtbl.find db slug in
    Stats.add_access stats;
    model

  let get_opt slug =
    Hashtbl.find_opt db slug >>= fun (stats, model) ->
    Stats.add_access stats;
    Some model

  let get_all () =
    Hashtbl.to_seq_values db
    |> Seq.map
      (fun (stats, model) ->
         Stats.add_access stats;
         model)

  let save ?slug ~name create =
    let slug =
      match slug with
      | None -> find_uniq_slug name
      | Some slug -> slug
    in
    let model = create slug in
    let json = Model.serialize (create slug) in
    let json = Json.remove_field "slug" json in
    let json =
      Model.separated_files
      |> List.fold_left
        (fun json file ->
           let field = Filename.chop_extension file in
           let content = Json.(get ~k:string [field] json) in
           Storage.write_entry_file Model.prefix slug file content;
           Json.remove_field field json)
        json
    in
    Storage.write_entry_json Model.prefix slug "meta.json" json;
    Storage.save_changes_on_entry
      ~msg:(spf "[auto] save %s / %s" Model.prefix slug)
      Model.prefix slug;
    Hashtbl.add db slug (Stats.empty (), model); (* FIXME: not add and not Stats.empty when editing. *)
    model

  let delete slug =
    Storage.delete_entry Model.prefix slug;
    Storage.save_changes_on_entry
      ~msg:(spf "[auto] delete %s / %s" Model.prefix slug)
      Model.prefix slug;
    Hashtbl.remove db slug
end
