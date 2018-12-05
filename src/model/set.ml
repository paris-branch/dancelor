open Dancelor_common
open Protocol_conv_jsonm
open Protocol_conv_yaml

type t =
  { slug : Slug.t ;
    name : string ;
    deviser : Slug.t ;
    kind : Kind.dance ;
    tunes : Slug.t list }
[@@deriving protocol ~driver:(module Jsonm),
            protocol ~driver:(module Yaml)]

module Database =
  struct
    let prefix = "set"

    let db = Hashtbl.create 8

    let initialise () =
      Storage.list_entries prefix
      |> List.iter
           (fun slug ->
             Storage.read_yaml prefix slug "meta.yaml"
             |> of_yaml
             |> Hashtbl.add db slug)

    let get = Hashtbl.find db

    let get_all () = Hashtbl.to_seq_values db |> List.of_seq
  end

type view =
  { slug : Slug.t ;
    name : string ;
    deviser : Credit.view ;
    kind : Kind.dance ;
    tunes : Tune.view list }
[@@deriving protocol ~driver:(module Jsonm)]

let view (set : t) =
  { slug = set.slug ;
    name = set.name ;
    deviser = Credit.(Database.get ||> view) set.deviser ;
    kind = set.kind ;
    tunes = List.map Tune.(Database.get ||> view) set.tunes }
