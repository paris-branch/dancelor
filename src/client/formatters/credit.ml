open Dancelor_client_html

module M = Dancelor_client_model
module Router = Dancelor_common.Router

let line ?(link=true) credit =
  match credit with
  | None -> Lwt.return_nil
  | Some credit ->
    let line_text = [text_lwt (M.Credit.line credit)] in
    if link then
      let href_lwt =
        let%lwt slug = M.Credit.slug credit in
        Lwt.return (Router.path_of_controller (Router.Credit slug) |> snd)
      in
      Lwt.return [a ~href_lwt line_text]
    else
      Lwt.return line_text
