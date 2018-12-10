open Dancelor_common
open Protocol_conv_jsonm

type t =
  { slug : Slug.t ;
    name : string }
[@@deriving to_protocol ~driver:(module Jsonm)]

let serialize person =
  `O [
      "slug", `String person.slug ;
      "name", `String person.name
    ]

let unserialize json =
  { slug = Slug.from_string Ezjsonm.(get_string (find json ["slug"])) ;
    name = Ezjsonm.(get_string (find json ["name"])) }

let slug p = p.slug
let name p = p.name

module Database =
  struct
    let prefix = "person"

    let db = Hashtbl.create 8

    let initialise () =
      let load entry =
        let json = Storage.read_entry_json prefix entry "meta.json" in
        let person = unserialize json in
        Hashtbl.add db person.slug person
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

    let get = Hashtbl.find db

    let create ~name () =
      let slug = find_uniq_slug name in
      let person = { slug ; name } in
      Hashtbl.add db slug person;
      serialize person
      |> Storage.write_entry_json prefix slug "meta.json";
      (slug, person)
  end
