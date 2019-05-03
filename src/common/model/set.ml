open Nes
open Protocol_conv_jsonm

type t =
  { slug : t Slug.t ;
    name : string ;
    deviser : Credit.t Slug.t option ;
    kind : Kind.dance ;
    status : Status.t ;
    tunes : Tune.t Slug.t list }
[@@deriving protocol ~driver:(module Jsonm)]

let to_jsonm = to_jsonm ||> Json.on_value (Json.add_field "type" (`String "set"))
let to_json = to_jsonm ||> Json.of_value

let of_json = Json.to_value ||> of_jsonm

let unserialize json =
  { slug = Json.(get ~k:slug ["slug"] json) ;
    name = Json.(get ~k:string ["name"] json) ;
    deviser = Json.(get_opt ~k:slug ["deviser"] json) ;
    kind = Kind.dance_of_string (Json.(get ~k:string ["kind"] json)) ;
    status = Json.(get ~k:string ["status"] json |> Status.from_string) ;
    tunes =
      NesOption.unwrap (
        Json.list (
          function
          | `String slug -> slug
          | _ -> failwith "Dancelor_model.Set.unserialize"
        )
          (Json.find ["tunes"] json)
      ) }

let serialize set =
  `O (
    [
      "slug", `String set.slug ;
      "name", `String set.name ;
      "kind", Kind.dance_to_jsonm set.kind ;
      "status", `String (Status.to_string set.status) ;
      "tunes", `A (List.map (fun tune -> `String tune) set.tunes)
    ]
    @ match set.deviser with
    | None -> []
    | Some deviser -> ["deviser", `String deviser]
  )

let slug s = s.slug
let name s = s.name
let kind s = s.kind
let tunes s = s.tunes
let deviser s = s.deviser
let contains t s = List.mem t s.tunes
