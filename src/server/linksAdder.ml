open Nes

(* FIXME: remove that as soon as we don't need Mustache anymore. *)

let out_of_slug prefix json =
  let slug = Slug.from_string Json.(get ~k:string ["slug"] json) in
  Json.add_field
    "link" (`String (prefix ^ "/" ^ slug))
    json

let link_adders =
  [ "set", (fun set ->
      let slug = Json.(get ~k:slug ["slug"] set) in
      let link ext = "/set/" ^ slug ^ ext in
      Json.add_fields
        ["link", `String (link "");
         "link_ly", `String (link ".ly");
         "link_pdf", `String (link ".pdf")]
        set) ;

    "credit", out_of_slug "/credit";

    "person", out_of_slug "/person";

    "program", (fun program ->
      let slug = Json.(get ~k:slug ["slug"] program) in
      let link ext = "/program/" ^ slug ^ ext in
      Json.add_fields
        ["link", `String (link "");
         "link_ly", `String (link ".ly");
         "link_pdf", `String (link ".pdf")]
        program) ;

    "tune-group", out_of_slug "/tune-group";

    "tune", (fun tune ->
      let slug = Json.(get ~k:slug ["slug"] tune) in
      let link ext = "/tune/" ^ slug ^ ext in
      tune
      |> Json.add_fields
           ["link", `String (link "");
            "link_ly", `String (link ".ly");
            "link_png", `String (link ".png")]) ]

let rec json_add_links json =
  match json with
  | `O fields ->
     (
       let json = `O (List.map (fun (field, value) -> (field, json_add_links value)) fields) in
       match Json.(get_opt ~k:string ["type"] json) with
       | Some type_ -> Json.to_value (List.assoc type_ link_adders json)
       | None -> json
     )
  | `A jsons ->
     `A (List.map json_add_links jsons)
  | _ ->
     json

let json_add_links json =
  Json.of_value (json_add_links json)
