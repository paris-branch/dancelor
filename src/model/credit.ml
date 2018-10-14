open Dancelor_common
open Protocol_conv_jsonm
open Protocol_conv_yaml

type t =
  { slug : Slug.t ;
    line : string ;
    persons : Slug.t list }
[@@deriving protocol ~driver:(module Jsonm),
            protocol ~driver:(module Yaml)]

module Database =
  struct
    let prefix = "credit"

    let db = Hashtbl.create 8

    let initialise () =
      Storage.list_entries prefix
      |> List.iter
           (fun slug ->
             Storage.read_yaml prefix slug "meta.yaml"
             |> of_jsonm
             |> Hashtbl.add db slug)

    let get = Hashtbl.find db
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
