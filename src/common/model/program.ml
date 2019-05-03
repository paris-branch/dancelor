open Nes
open Protocol_conv_jsonm

type t =
  { slug : t Slug.t ;
    name : string ;
    date : Date.t ;
    status : Status. t ;
    sets : Set.t Slug.t list }
[@@deriving to_protocol ~driver:(module Jsonm)]

let to_jsonm = to_jsonm ||> Json.on_value (Json.add_field "type" (`String "program"))
let to_json = to_jsonm ||> Json.of_value

let unserialize json =
  { slug = Json.(get ~k:slug ["slug"] json) ;
    name = Json.(get ~k:string ["name"] json) ;
    date = Json.(get ~k:string ["date"] json |> Date.from_string) ;
    status = Json.(get ~k:string ["status"] json |> Status.from_string) ;
    sets =
      NesOption.unwrap (
        Json.list (
          function
          | `String slug -> Slug.from_string slug
          | _ -> failwith "Dancelor_model.Program.unserialize"
        ) (Json.find ["sets"] json)
      )
  }

let serialize p =
  `O (
      [
        "slug", `String p.slug ;
        "name", `String p.name ;
        "date", `String (Date.to_string p.date) ;
        "status", `String (Status.to_string p.status) ;
        "sets", `A (List.map (fun slug -> `String slug) p.sets)
      ]
    )

let slug p = p.slug
let date p = p.date

let contains s p =
  List.mem s p.sets

let compare p1 p2 =
  (* Compare first by date *)
  let c = compare p1.date p2.date in
  if c = 0 then
    compare p1 p2
  else
    c
