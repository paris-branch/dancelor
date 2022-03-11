open Dancelor_client_html

module M = Dancelor_client_model
module Router = Dancelor_common.Router

let name ?(link=true) dance =
  let name_lwt = M.Dance.name dance in
  if link then
    let href_lwt =
      let%lwt slug = M.Dance.slug dance in
      Lwt.return (Router.path_of_controller (Router.Dance slug) |> snd)
    in
    Lwt.return [ a ~href_lwt [ text_lwt name_lwt ] ]
  else
    Lwt.return [ text_lwt name_lwt ]