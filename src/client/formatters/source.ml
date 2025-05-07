open Common

open Html

let name ?(link = true) source =
  let name_text = [txt (Model.Source.name' source)] in
  if link then
    [
      a
        ~a: [a_href @@ Endpoints.Page.href_source @@ Entry.slug source]
        name_text
    ]
  else
    name_text
