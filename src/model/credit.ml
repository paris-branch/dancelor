open Dancelor_common
open Protocol_conv_jsonm

type t =
  { slug : Slug.t ;
    line : string ;
    persons : Person.t list }
[@@deriving to_protocol ~driver:(module Jsonm)]

let to_jsonm =
  to_jsonm
  ||> JsonHelpers.add_field "type" (`String "credit")

let serialize credit =
  `O [
      "slug", `String credit.slug;
      "line", `String credit.line;
      "persons", `A (List.map (fun person -> `String (Person.slug person)) credit.persons)
    ]

let unserialize json =
  let slug = Serializer.(get ~type_:slug ["slug"] json) in
  let persons =
    Serializer.get_or
      ~type_:(Ezjsonm.get_strings ||> List.map (Slug.from_string ||> Person.Database.get))
      ~default:[]
      ["persons"]
      json
  in
  let line =
    try
      Serializer.(get ~type_:string ["line"] json)
    with
      Not_found ->
       match persons with
       | [person] -> Person.name person
       | _ -> raise Not_found
  in
  { slug ; line ; persons }

let slug c = c.slug
let line c = c.line

module Database = struct
  module Log = (val Log.create "dancelor.model.credit.database" : Logs.LOG)

    let prefix = "credit"

    let db = Hashtbl.create 8

    let initialise () =
      let load entry =
        let json = Storage.read_entry_json prefix entry "meta.json" in
        let credit = unserialize json in
        Hashtbl.add db credit.slug credit
      in
      Storage.list_entries prefix
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

    let get slug =
      Log.debug (fun m -> m "Looking for %s" slug);
      Hashtbl.find db slug

    let create ~line ?(persons=[]) () =
      let slug = find_uniq_slug line in
      let credit = { slug ; line ; persons } in
      Hashtbl.add db slug credit;
      serialize credit
      |> Storage.write_entry_json prefix slug "meta.json";
      (slug, credit)
  end
