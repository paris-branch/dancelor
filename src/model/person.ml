open Dancelor_common
open Protocol_conv_jsonm

type t =
  { slug : Slug.t ;
    name : string }
[@@deriving protocol ~driver:(module Jsonm)]

let slug p = p.slug

module Database =
  struct
    let prefix = "person"

    let db = Hashtbl.create 8

    let initialise () =
      Storage.list_entries prefix
      |> List.iter
           (fun slug ->
             Storage.read_json prefix slug "meta.json"
             |> of_jsonm
             |> Hashtbl.add db slug)

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
      to_jsonm person
      |> Storage.write_json prefix slug "meta.json";
      (slug, person)
  end

type view = t =
  { slug : Slug.t ;
    name : string }
[@@deriving protocol ~driver:(module Jsonm)]

let view person = person
