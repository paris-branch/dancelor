open Dancelor_client_html

module M = Dancelor_client_model
module R = Dancelor_client_router

let name ?(link=true) dance =
  let name_lwt = M.Dance.name dance in
  if link then
    let href_lwt =
      let%lwt slug = M.Dance.slug dance in
      Lwt.return R.(path (Dance slug))
    in
    Lwt.return [ a ~href_lwt [ text_lwt name_lwt ] ]
  else
    Lwt.return [ text_lwt name_lwt ]
