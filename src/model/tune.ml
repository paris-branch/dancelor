open Dancelor_common
open Protocol_conv_jsonm
open Protocol_conv_yaml

type t =
  { slug : Slug.t ;
    name : string ;
    kind : Kind.tune ;
    credit : Slug.t ;
    content : string }
[@@deriving protocol ~driver:(module Jsonm),
            protocol ~driver:(module Yaml)]

module Database =
  struct
    let prefix = "tune"

    let db = Hashtbl.create 8

    let initialise =
      let initialised = ref false in
      fun () ->
      if not !initialised then
        (
          Storage.list_entries prefix
          |> List.iter
               (fun slug ->
                 Storage.read_yaml prefix slug "meta.yaml"
                 |> of_yaml
                 |> Hashtbl.add db slug);
          initialised := true
        )

    let get = Hashtbl.find db
  end

type view =
  { slug : Slug.t ;
    name : string ;
    kind : Kind.tune ;
    credit : Credit.view ;
    content : string }
[@@deriving protocol ~driver:(module Jsonm)]

let view ({ slug ; name ; kind ; credit ; content } : t) =
  { slug ; name ; kind ;
    credit = Credit.(Database.get ||> view) credit ;
    content }
