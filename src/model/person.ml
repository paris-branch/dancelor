open Dancelor_common
open Protocol_conv_jsonm
open Protocol_conv_yaml

type t =
  { slug : Slug.t ;
    name : string }
[@@deriving protocol ~driver:(module Jsonm),
            protocol ~driver:(module Yaml)]

let make ?slug ~name () =
  let slug =
    match slug with
    | None -> Slug.from_string name
    | Some slug -> slug
  in
  { slug ; name }

let slug p = p.slug

module Database =
  struct
    let prefix = "person"

    let db = Hashtbl.create 8

    let initialise () =
      Storage.list_entries prefix
      |> List.iter
           (fun slug ->
             Storage.read_yaml prefix slug "meta.yaml"
             |> of_yaml
             |> Hashtbl.add db slug)

    let get = Hashtbl.find db
  end

type view = t =
  { slug : Slug.t ;
    name : string }
[@@deriving protocol ~driver:(module Jsonm)]

let view person = person
