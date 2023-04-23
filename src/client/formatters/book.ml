open Dancelor_client_html

module M = Dancelor_client_model
module Router = Dancelor_client_router

let title_and_subtitle ?(link=true) book =
  let title_text = [text_lwt (M.Book.title book)] in
  if link then
    let%lwt subtitle_block =
      match%lwt M.Book.subtitle book with
      | "" -> Lwt.return_nil
      | subtitle -> Lwt.return [
          span ~classes:["details"] [text subtitle]
        ]
    in
    Lwt.return (title_text @ subtitle_block)
  else
    Lwt.return title_text

let short_title ?(link=true) book =
  let short_title_text = [text_lwt (M.Book.short_title book)] in
  if link then
    let href_lwt =
      let%lwt slug = M.Book.slug book in
      Lwt.return Router.(path (Book slug))
    in
    Lwt.return [a ~href_lwt short_title_text]
  else
    Lwt.return short_title_text
