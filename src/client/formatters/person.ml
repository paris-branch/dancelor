open Dancelor_common
open Dancelor_client_html
module M = Dancelor_client_model

let line ?(link=true) person =
  match person with
  | None -> Lwt.return_nil
  | Some person ->
    let line_text = [L.txt (M.Person.line person)] in
    if link then
      let href_lwt =
        let%lwt slug = M.Person.slug person in
        Lwt.return PageRouter.(path (Person slug))
      in
      Lwt.return [
        a
          ~a:[L.a_href href_lwt]
          line_text
      ]
    else
      Lwt.return line_text
