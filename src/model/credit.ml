open Dancelor_common
open Protocol_conv_jsonm

type t =
  { slug : Slug.t ;
    credit : string ;
    persons : Slug.t list }
[@@deriving protocol ~driver:(module Jsonm)]

type view =
  { slug : Slug.t ;
    credit : string ;
    persons : Person.view list }
[@@deriving protocol ~driver:(module Jsonm)]

let view ({ slug ; credit ; persons } : t) =
  { slug ; credit ;
    persons = List.map (Person.(Database.get ||> view)) persons }

module Database =
  struct
    let prefix = "credit"

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
