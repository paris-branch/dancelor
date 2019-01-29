open Dancelor_common open Option
open Protocol_conv_jsonm

type t =
  { slug : Slug.t ;
    name : string ;
    kind : Kind.base ;
    author : Credit.t option ;
    remark : string }
[@@deriving to_protocol ~driver:(module Jsonm)]

let to_jsonm tune_group =
  to_jsonm tune_group
  |> Json.of_value
  |> Json.add_field "type" (`String "tune-group")
  |> Json.to_value

let to_json = to_jsonm ||> Json.of_value

let unserialize json =
  { slug = Json.(get ~k:slug ["slug"] json) ;
    name = Json.(get ~k:string ["name"] json) ;
    kind = Json.(get ~k:(string >=> (Kind.base_of_string ||> wrap)) ["kind"] json) ;
    remark = Json.(get_or ~k:string ~default:"" ["remark"] json) ;
    author = Json.(get_opt ~k:(slug >=> (Credit.Database.get ||> wrap)) ["author"] json) }

let slug tune_group = tune_group.slug
let name tune_group = tune_group.name
let kind tune_group = tune_group.kind
let author tune_group = tune_group.author

module Database =
  struct
    let prefix = "tune-group"

    let db = Hashtbl.create 8

    let initialise () =
      let load entry =
        let json = Storage.read_entry_json prefix entry "meta.json" in
        let tune_group = unserialize json in
        Hashtbl.add db tune_group.slug tune_group
      in
      Storage.list_entries prefix
      |> List.iter load

    let get = Hashtbl.find db
  end
