open Dancelor_common open Option
open Protocol_conv_jsonm

type t =
  { slug : t Slug.t ;
    group : TuneGroup.t Slug.t ;
    bars : int ;
    key : Music.key ;
    structure : string ;
    arranger : Credit.t Slug.t option ;
    content : string }
[@@deriving protocol ~driver:(module Jsonm)]

let to_jsonm tune =
  to_jsonm tune
  |> Json.of_value
  |> Json.add_field "type" (`String "tune")
  |> Json.to_value

let to_json = to_jsonm ||> Json.of_value

let of_json = Json.to_value ||> of_jsonm

let serialize tune =
  `O (
    [
      "slug", `String tune.slug ;
      "group", `String tune.group ;
      "bars", `Float (float_of_int tune.bars) ;
      "key", `String (Music.key_to_string tune.key) ;
      "structure", `String tune.structure ;
      "content", `String tune.content ;
    ] @ match tune.arranger with
    | None -> []
    | Some arranger -> ["arranger", `String arranger]
  )

let unserialize json =
  { slug = Json.(get ~k:slug ["slug"] json) ;
    group = Json.(get ~k:slug ["tune-group"] json) ;
    bars = Json.(get ~k:int ["bars"] json) ;
    key = Json.(get ~k:(string >=> (Music.key_of_string ||> wrap)) ["key"] json) ;
    structure = Json.(get ~k:string ["structure"] json) ;
    arranger = Json.(get_opt ~k:slug ["arranger"] json) ;
    content = Json.(get ~k:string ["content"] json) }

let slug tune = tune.slug
let group tune = tune.group
let key tune = tune.key
let content tune = tune.content
let bars tune = tune.bars
let structure tune = tune.structure
