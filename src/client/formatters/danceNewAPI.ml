open Dancelor_common
open Dancelor_client_html.NewAPI
module M = Dancelor_client_model

let name ?(link=true) dance =
  let name_text = [R.txt @@ S.from' ~placeholder:"" @@ M.Dance.name dance] in
  if link then
    let href =
      let%lwt slug = M.Dance.slug dance in
      Lwt.return PageRouter.(path (Dance slug))
    in
    Lwt.return [a ~a:[R.a_href @@ S.from' ~placeholder:"" href] name_text]
  else
    Lwt.return name_text
