open Nes
open Dancelor_common
open Dancelor_client_html.NewAPI
module M = Dancelor_client_model

let name ?(link=true) tune =
  let name_text = [R.txt @@ S.from' ~placeholder:"" @@ M.Tune.name tune] in
  if link then
    let href =
      let%lwt slug = M.Tune.slug tune in
      Lwt.return PageRouter.(path (Tune slug))
    in
    Lwt.return [a ~a:[R.a_href @@ S.from' ~placeholder:"" href] name_text]
  else
    Lwt.return name_text
