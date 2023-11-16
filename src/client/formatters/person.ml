open Dancelor_common
open Dancelor_client_html
module M = Dancelor_client_model

let name ?(link=true) person =
  match person with
  | None -> []
  | Some person ->
    let name_text = [txt (M.Person.name person)] in
    if link then
      [
        a
          ~a:[a_href (PageRouter.path (PageRouter.Person (M.Person.slug person)))]
          name_text
      ]
    else
      name_text
