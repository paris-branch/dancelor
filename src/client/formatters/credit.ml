open Dancelor_common
open Dancelor_client_html

module M = Dancelor_client_model

let line ?(link=true) credit =
  match credit with
  | None -> Lwt.return_nil
  | Some credit ->
    let line_text = [text lwt (M.Credit.line credit)] in
    if link then
      let href_lwt =
        let%lwt slug = M.Credit.slug credit in
        Lwt.return PageRouter.(path (Credit slug))
      in
      Lwt.return [a ~href_lwt const line_text]
    else
      Lwt.return line_text
