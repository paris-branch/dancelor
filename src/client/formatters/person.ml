open Dancelor_common
open Dancelor_client_html
module M = Dancelor_client_model

let name ?(link=true) person =
  match person with
  | None -> Lwt.return_nil
  | Some person ->
    let name_text = [txt (M.Person.name person)] in
    if link then
      let href_lwt =
        let slug = M.Person.slug person in
        Lwt.return PageRouter.(path (Person slug))
      in
      Lwt.return [
        a
          ~a:[L.a_href href_lwt]
          name_text
      ]
    else
      Lwt.return name_text
