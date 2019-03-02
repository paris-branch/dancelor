open Dancelor_common open Option
open Protocol_conv_jsonm

type t =
  { slug : Slug.t ;
    line : string ;
    persons : Person.t list }
[@@deriving protocol ~driver:(module Jsonm)]

let to_jsonm = to_jsonm ||> Json.on_value (Json.add_field "type" (`String "credit"))
let to_json = to_jsonm ||> Json.of_value

let of_json = Json.to_value ||> of_jsonm

let serialize credit =
  `O [
      "slug", `String credit.slug;
      "line", `String credit.line;
      "persons", `A (List.map (fun person -> `String (Person.slug person)) credit.persons)
    ]

let unserialize json =
  let slug = Json.(get ~k:slug ["slug"] json) in
  let persons =
    Json.get_or
      ~k:(Json.strings >=> (List.map (Slug.from_string ||> Person.Database.get_opt ||> unwrap) ||> wrap))
      ~default:[]
      ["persons"]
      json
  in
  let line =
    try
      Json.(get ~k:string ["line"] json)
    with
      Not_found ->
       match persons with
       | [person] -> Person.name person
       | _ -> raise Not_found
  in
  { slug ; line ; persons }

let slug c = c.slug
let line c = c.line

module Database = struct
  include GenericDatabase.Make
      (val Log.create "dancelor.model.credit.database" : Logs.LOG)
      (struct
        type nonrec t = t
        let slug = slug

        let serialize = serialize
        let unserialize = unserialize

        let prefix = "credit"
        let separated_files = []
      end)

  let save ?slug ~line ?(persons=[]) () =
    save ?slug ~name:line @@ fun slug ->
    { slug ; line ; persons }
end
