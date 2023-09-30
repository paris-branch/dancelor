open Nes
open Dancelor_common
open Dancelor_client_html.NewAPI
module M = Dancelor_client_model

let name ?(link=true) version =
  let name_text = [R.txt @@ S.from' ~placeholder:"" (M.Version.tune version >>=| M.Tune.name)] in
  if link then
    let href =
      let%lwt slug = M.Version.slug version in
      Lwt.return PageRouter.(path (Version slug))
    in
    Lwt.return [a ~a:[R.a_href @@ S.from' ~placeholder:"" href] name_text]
  else
    Lwt.return name_text
