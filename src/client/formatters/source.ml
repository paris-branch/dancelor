open Nes
open Common

open Html

let name ?(short = false) source =
  let name =
    match short, Model.Source.short_name source with
    | false, _ | true, None -> Model.Source.name source
    | true, Some short_name -> short_name
  in
  span [txt @@ NEString.to_string name]

let name' ?(short = false) ?(link = true) source =
  let name =
    match short, Model.Source.short_name' source with
    | false, _ | true, None -> Model.Source.name' source
    | true, Some short_name -> short_name
  in
  let name_text = [txt @@ NEString.to_string name] in
  span @@
    if link then
      [
        a
          ~a: [a_href @@ Endpoints.Page.href_source @@ Entry.id source]
          name_text
      ]
    else
      name_text

let date_and_editors source =
  with_span_placeholder @@ (
    let date =
      match Model.Source.date source with
      | None -> []
      | Some date -> [txt (spf "Published %s" (NesPartialDate.to_pretty_string ~at: true date))]
    in
    let%lwt editors =
      match%lwt Model.Source.editors source with
      | [] -> lwt_nil
      | editors -> lwt [txt "by "; Person.names' ~links: true editors]
    in
    lwt (date @ [txt " "] @ editors)
  )

let date_and_editors' source = date_and_editors @@ Entry.value source
