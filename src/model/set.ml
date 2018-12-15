open Dancelor_common
open Protocol_conv_jsonm

type t =
  { slug : Slug.t ;
    name : string ;
    deviser : Credit.t ;
    kind : Kind.dance ;
    tunes : Tune.tune_version list }
[@@deriving to_protocol ~driver:(module Jsonm)]

let to_jsonm =
  to_jsonm
  ||> Json.add_field "type" (`String "set")

let unserialize json =
  let open Ezjsonm in
  { slug = Slug.from_string (get_string (find json ["slug"])) ;
    name = get_string (find json ["name"]) ;
    deviser = Credit.Database.get (Slug.from_string (get_string (find json ["deviser"]))) ;
    kind = Kind.dance_of_string (get_string (find json ["kind"])) ;
    tunes =
      get_list (
          function
          | `String slug ->
             let tune = Tune.Database.get slug in
             (tune, Tune.default_version tune)
          | `A [`String slug; `String subslug] ->
             let tune = Tune.Database.get slug in
             (tune, Tune.version tune subslug)
          | _ -> failwith "Dancelor_model.Set.unserialize"
        ) (find json ["tunes"]) }

module Database =
  struct
    let prefix = "set"

    let db = Hashtbl.create 8

    let initialise () =
      let load entry =
        let json = Storage.read_entry_json prefix entry "meta.json" in
        let set = unserialize json in
        Hashtbl.add db set.slug set
      in
      Storage.list_entries prefix
      |> List.iter load

    let get = Hashtbl.find db

    let get_all () = Hashtbl.to_seq_values db |> List.of_seq
  end
