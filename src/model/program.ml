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
    let prefix = "program"

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
    let get_opt = Hashtbl.find_opt db

    let get_all () = Hashtbl.to_seq_values db |> List.of_seq
end
