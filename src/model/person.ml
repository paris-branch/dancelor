open Dancelor_common
open Protocol_conv_jsonm

type t =
  { slug : Slug.t ;
    name : string }
[@@deriving protocol ~driver:(module Jsonm)]

let to_jsonm = to_jsonm ||> Json.on_value (Json.add_field "type" (`String "person"))
let to_json = to_jsonm ||> Json.of_value

let of_json = Json.to_value ||> of_jsonm

let serialize person =
  `O [
      "slug", `String person.slug ;
      "name", `String person.name
    ]

let unserialize json =
  { slug = Slug.from_string Json.(get ~k:string ["slug"] json) ;
    name = Json.(get ~k:string ["name"] json) }

let slug p = p.slug
let name p = p.name

module Database = struct
  include GenericDatabase.Make
      (val Log.create "dancelor.model.person.database" : Logs.LOG)
      (struct
        type nonrec t = t
        let slug = slug

        let serialize = serialize
        let unserialize = unserialize

        let prefix = "person"
        let separated_files = []
      end)

  let save ?slug ~name () =
    save ?slug ~name @@ fun slug ->
    { slug ; name }
end
