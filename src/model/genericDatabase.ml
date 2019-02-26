open Dancelor_common

module type Model = sig
  type t
  val slug : t -> Slug.t
  val serialize : t -> Json.t
  val unserialize : Json.t -> t

  val prefix : string
end

module Make (Model : Model) = struct
  let db = Hashtbl.create 8

  let initialise () =
    let load entry =
      let json = Storage.read_entry_json Model.prefix entry "meta.json" in
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
    let json = Model.serialize model in
    Storage.write_entry_json Model.prefix slug "meta.json" json;
    Hashtbl.add db slug model;
    model

  let delete set =
    Storage.delete_entry Model.prefix (Model.slug set);
    Hashtbl.remove db (Model.slug set)
end
