open Dancelor_common open Option
open Protocol_conv_jsonm

type t =
  { slug : Slug.t ;
    name : string ;
    sets : Set.t list }
[@@deriving to_protocol ~driver:(module Jsonm)]

let to_jsonm = to_jsonm ||> Json.on_value (Json.add_field "type" (`String "program"))
let to_json = to_jsonm ||> Json.of_value

let unserialize json =
  { slug = Json.(get ~k:slug ["slug"] json) ;
    name = Json.(get ~k:string ["name"] json) ;
    sets =
      unwrap (
          Json.list (
              function
              | `String slug -> Some (Set.Database.get slug)
              | _ -> failwith "Dancelor_model.Program.unserialize"
            ) (Json.find ["sets"] json)
  ) }

let serialize p =
  `O (
      [
        "slug", `String p.slug ;
        "name", `String p.name ;
        "sets", `A (List.map (fun set -> `String (Set.slug set)) p.sets)
      ]
    )

let slug p = p.slug

module Database = struct
  include GenericDatabase.Make (
    struct
      type nonrec t = t
      let slug = slug

      let serialize = serialize
      let unserialize = unserialize

      let prefix = "program"
      let separated_files = []
    end)
end
