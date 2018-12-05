open Dancelor_common
open Protocol_conv_jsonm
open Protocol_conv_yaml

type t =
  { slug : Slug.t ;
    name : string ;
    kind : Kind.tune ;
    author : Slug.t ;
    content : string }
[@@deriving protocol ~driver:(module Jsonm),
            protocol ~driver:(module Yaml)]

module Database =
  struct
    let prefix = "tune"

    let db = Hashtbl.create 8

    let initialise () =
      Storage.list_entries prefix
      |> List.iter
           (fun slug ->
             Storage.read_yaml prefix slug "meta.yaml"
             |> of_yaml
             |> Hashtbl.add db slug)

    let get = Hashtbl.find db

    let get_all () =
      Hashtbl.to_seq_values db |> List.of_seq
  end

type view =
  { slug : Slug.t ;
    name : string ;
    author : Credit.view ;
    kind : Kind.tune ;
    (* content : string *) }
[@@deriving protocol ~driver:(module Jsonm)]

let view (tune : t) =
  { slug = tune.slug ;
    name = tune.name ;
    author = Credit.(Database.get ||> view) tune.author ;
    kind = tune.kind ;
    (* content = tune.content *) }
