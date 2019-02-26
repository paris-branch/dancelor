open Dancelor_common open Option
open Protocol_conv_jsonm

type t =
  { slug : Slug.t ;
    name : string ;
    kind : Kind.base ;
    author : Credit.t option ;
    remark : string }
[@@deriving protocol ~driver:(module Jsonm)]

let to_jsonm tune_group =
  to_jsonm tune_group
  |> Json.of_value
  |> Json.add_field "type" (`String "tune-group")
  |> Json.to_value

let to_json = to_jsonm ||> Json.of_value

let of_json = Json.to_value ||> of_jsonm

let serialize tune_group =
  `O (
    [
      "slug", `String tune_group.slug ;
      "name", `String tune_group.name ;
      "kind", `String (Kind.base_to_string tune_group.kind) ;
      "remark", `String tune_group.remark ;
    ] @ match tune_group.author with
    | None -> []
    | Some author -> ["author", `String (Credit.slug author)]
  )

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

module Database = struct
  include GenericDatabase.Make (
    struct
      type nonrec t = t
      let slug = slug

      let serialize = serialize
      let unserialize = unserialize

      let prefix = "tune-group"
      let separated_files = []
    end)
end
