open Dancelor_common
open Dancelor_client_html
module M = Dancelor_client_model

let name ?(link=true) dance =
  let name_text = [L.txt (M.Dance.name dance)] in
  if link then
    let href =
      let%lwt slug = M.Dance.slug dance in
      Lwt.return PageRouter.(path (Dance slug))
    in
    Lwt.return [a ~a:[L.a_href href] name_text]
  else
    Lwt.return name_text
