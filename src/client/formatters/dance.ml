open Common

open Html

let name ?(link = true) dance =
  let name_text = [txt (Model.Dance.name dance)] in
  if link then
    [
      a
        ~a: [a_href @@ Endpoints.Page.href_dance @@ Entry.slug dance]
        name_text
    ]
  else
    name_text
