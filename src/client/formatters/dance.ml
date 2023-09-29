open Dancelor_common
open Dancelor_client_html

module M = Dancelor_client_model

let name ?(link=true) dance =
  let name_lwt = M.Dance.name dance in
  if link then
    let href_lwt =
      let%lwt slug = M.Dance.slug dance in
      Lwt.return PageRouter.(path (Dance slug))
    in
    Lwt.return [a ~href_lwt const [text lwt name_lwt]]
  else
    Lwt.return [text lwt name_lwt]
