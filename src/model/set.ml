open Dancelor_common open Option
open Protocol_conv_jsonm

type t =
  { slug : Slug.t ;
    name : string ;
    deviser : Credit.t ;
    kind : Kind.dance ;
    tunes : Tune.tune_version list }
[@@deriving to_protocol ~driver:(module Jsonm)]

let to_jsonm = to_jsonm ||> Json.on_value (Json.add_field "type" (`String "set"))
let to_json = to_jsonm ||> Json.of_value

let unserialize json =
  { slug = Json.(get ~k:slug ["slug"] json) ;
    name = Json.(get ~k:string ["name"] json) ;
    deviser = Credit.Database.get (Json.(get ~k:slug ["deviser"] json)) ;
    kind = Kind.dance_of_string (Json.(get ~k:string ["kind"] json)) ;
    tunes =
      unwrap (
          Json.list (
              function
              | `String slug ->
                 let tune = Tune.Database.get slug in
                 Some (tune, Tune.default_version tune)
              | `A [`String slug; `String subslug] ->
                 let tune = Tune.Database.get slug in
                 Some (tune, Tune.version tune subslug)
              | _ -> failwith "Dancelor_model.Set.unserialize"
            ) (Json.find ["tunes"] json)
  ) }

module Database = struct
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
