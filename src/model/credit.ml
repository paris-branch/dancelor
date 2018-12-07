open Dancelor_common
open Protocol_conv_jsonm

type t =
  { slug : Slug.t ;
    line : string ;
    persons : Slug.t list }
[@@deriving protocol ~driver:(module Jsonm)]

let slug c = c.slug
let line c = c.line

module Database =
  struct
    let prefix = "credit"

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

    let create ~line ?(persons=[]) () =
      let slug = find_uniq_slug line in
      let persons = List.map Person.slug persons in
      let credit = { slug ; line ; persons } in
      Hashtbl.add db slug credit;
      to_jsonm credit
      |> Storage.write_json prefix slug "meta.json";
      (slug, credit)
  end

type view =
  { slug : Slug.t ;
    line : string ;
    persons : Person.view list }
[@@deriving protocol ~driver:(module Jsonm)]

let view (credit : t) =
  { slug = credit.slug ;
    line = credit.line ;
    persons = List.map (Person.(Database.get ||> view)) credit.persons }
