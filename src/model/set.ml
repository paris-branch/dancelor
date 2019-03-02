open Dancelor_common open Option
open Protocol_conv_jsonm

type t =
  { slug : Slug.t ;
    name : string ;
    deviser : Credit.t option ;
    kind : Kind.dance ;
    tunes : Tune.t list }
[@@deriving protocol ~driver:(module Jsonm)]

let to_jsonm = to_jsonm ||> Json.on_value (Json.add_field "type" (`String "set"))
let to_json = to_jsonm ||> Json.of_value

let of_json = Json.to_value ||> of_jsonm

let unserialize json =
  { slug = Json.(get ~k:slug ["slug"] json) ;
    name = Json.(get ~k:string ["name"] json) ;
    deviser = (Json.(get_opt ~k:slug ["deviser"] json) >>= fun slug -> assert_some (Credit.Database.get_opt slug)) ;
    kind = Kind.dance_of_string (Json.(get ~k:string ["kind"] json)) ;
    tunes =
      unwrap (
        Json.list (
          function
          | `String slug -> assert_some (Tune.Database.get_opt slug)
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
      "tunes", `A (List.map (fun tune -> `String (Tune.slug tune)) set.tunes)
    ]
    @ match set.deviser with
    | None -> []
    | Some deviser -> ["deviser", `String (Credit.slug deviser)]
  )

let slug s = s.slug
let name s = s.name
let kind s = s.kind
let tunes s = s.tunes
let deviser s = s.deviser

module Database = struct
  include GenericDatabase.Make
      (val Log.create "dancelor.model.set.database" : Logs.LOG)
      (struct
        type nonrec t = t
        let slug = slug

        let serialize = serialize
        let unserialize = unserialize

        let prefix = "set"
        let separated_files = []
      end)

  let save ?slug ~name ?deviser ~kind ~tunes () =
    save ?slug ~name @@ fun slug ->
    { slug; name; deviser; kind; tunes }
end
