open Dancelor_common
open Dancelor_client_html
module M = Dancelor_client_model

let name ?(link = true) dance =
  let name_text = [txt (M.Dance.name dance)] in
  if link then
    [
      a
        ~a: [a_href @@ PageRouter.href_dance @@ Database.Entry.slug dance]
        name_text
    ]
  else
    name_text
