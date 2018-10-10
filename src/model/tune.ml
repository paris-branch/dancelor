open Dancelor_common
open Protocol_conv_jsonm

type t =
  { slug : Slug.t ;
    name : string ;
    kind : Kind.tune ;
    credit : Slug.t }
[@@deriving protocol ~driver:(module Jsonm)]

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
                 Storage.read_json prefix slug "meta.json"
                 |> of_jsonm
                 |> Hashtbl.add db slug);
          initialised := true
        )

    let get = Hashtbl.find db
  end

type view =
  { slug : Slug.t ;
    name : string ;
    kind : Kind.tune ;
    credit : Credit.view }
[@@deriving protocol ~driver:(module Jsonm)]

let view ({ slug ; name ; kind ; credit } : t) =
  { slug ; name ; kind ;
    credit = Credit.(Database.get ||> view) credit }
