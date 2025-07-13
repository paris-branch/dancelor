open Nes
open Common

open Html

let name ?(short = false) source =
  let name =
    match short, Model.Source.short_name source with
    | false, _ | true, "" -> Model.Source.name source
    | true, short_name -> short_name
  in
  span [txt name]

let name' ?(short = false) ?(link = true) source =
  let name =
    match short, Model.Source.short_name' source with
    | false, _ | true, "" -> Model.Source.name' source
    | true, short_name -> short_name
  in
  let name_text = [txt name] in
  span @@
    if link then
      [
        a
          ~a: [a_href @@ Endpoints.Page.href_source @@ Entry.slug source]
          name_text
      ]
    else
      name_text

let editors source =
  with_span_placeholder @@
    match%lwt Model.Source.editors source with
    | [] -> lwt_nil
    | editors -> lwt [txt "by "; Person.names' ~links: true editors]

let editors' source = editors @@ Entry.value source
