open Dancelor_common

module type Model = sig
  type t
  val slug : t -> Slug.t
  val serialize : t -> Json.t
  val unserialize : Json.t -> t

  val prefix : string
  val separated_files : string list
end

module Make (Model : Model) = struct
  let db = Hashtbl.create 8

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
      Hashtbl.add db (Model.slug model) model
    in
    Storage.list_entries Model.prefix
    |> List.iter load

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

  let get = Hashtbl.find db
  let get_opt = Hashtbl.find_opt db
  let get_all () = Hashtbl.to_seq_values db |> List.of_seq

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
    Hashtbl.add db slug model;
    model

  let delete set =
    Storage.delete_entry Model.prefix (Model.slug set);
    Hashtbl.remove db (Model.slug set)
end
