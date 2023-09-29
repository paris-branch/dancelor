open Dancelor_common
open Dancelor_client_html

module M = Dancelor_client_model

let title_and_subtitle ?(link=true) book =
  let title_text = [text lwt (M.Book.title book)] in
  if link then
    let%lwt subtitle_block =
      match%lwt M.Book.subtitle book with
      | "" -> Lwt.return_nil
      | subtitle -> Lwt.return [
          span ~classes:["details"] const [text const subtitle]
        ]
    in
    Lwt.return (title_text @ subtitle_block)
  else
    Lwt.return title_text

let short_title ?(link=true) book =
  let short_title_text = [text lwt (M.Book.short_title book)] in
  if link then
    let href_lwt =
      let%lwt slug = M.Book.slug book in
      Lwt.return PageRouter.(path (Book slug))
    in
    Lwt.return [a ~href_lwt const short_title_text]
  else
    Lwt.return short_title_text
