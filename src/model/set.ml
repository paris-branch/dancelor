open Dancelor_common open Option
open Protocol_conv_jsonm

type t =
  { slug : Slug.t ;
    name : string ;
    deviser : Credit.t option ;
    kind : Kind.dance ;
    tunes : Tune.t list }
[@@deriving to_protocol ~driver:(module Jsonm)]

let to_jsonm = to_jsonm ||> Json.on_value (Json.add_field "type" (`String "set"))
let to_json = to_jsonm ||> Json.of_value

let unserialize json =
  { slug = Json.(get ~k:slug ["slug"] json) ;
    name = Json.(get ~k:string ["name"] json) ;
    deviser = (Json.(get_opt ~k:slug ["deviser"] json) >>= fun slug -> Some (Credit.Database.get slug)) ;
    kind = Kind.dance_of_string (Json.(get ~k:string ["kind"] json)) ;
    tunes =
      unwrap (
          Json.list (
              function
              | `String slug -> Some (Tune.Database.get slug)
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

module Database = struct
    let prefix = "set"

    let db = Hashtbl.create 8

    let initialise () =
      let load entry =
        let json = Storage.read_entry_json prefix entry "meta.json" in
        let set = unserialize json in
        Hashtbl.add db set.slug set
      in
      Storage.list_entries prefix
      |> List.iter load

    let find_uniq_slug string =
      let slug = Slug.from_string string in
      let rec aux i =
        let slug = slug ^ "-" ^ (string_of_int i) in
        if Hashtbl.mem db slug then
          aux (i+1)
        else
          slug
      in
      if Hashtbl.mem db slug then
        aux 2
      else
        slug

    let get = Hashtbl.find db
    let get_opt = Hashtbl.find_opt db

    let get_all () = Hashtbl.to_seq_values db |> List.of_seq

    let create ~name ?deviser ~kind ~tunes () =
      let slug = find_uniq_slug name in
      let set = { slug; name; deviser; kind; tunes } in
      let json = serialize set in
      Storage.write_entry_json prefix slug "meta.json" json;
      Hashtbl.add db slug set;
      set
end
