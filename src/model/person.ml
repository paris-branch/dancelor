open Dancelor_common
open Protocol_conv_jsonm

type t =
  { slug : Slug.t ;
    name : string }
[@@deriving protocol ~driver:(module Jsonm)]

module Database =
  struct
    let prefix = "person"

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

type view = t =
  { slug : Slug.t ;
    name : string }
[@@deriving protocol ~driver:(module Jsonm)]

let view person = person
