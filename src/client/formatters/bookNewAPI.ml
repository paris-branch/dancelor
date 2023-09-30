open Dancelor_common
open Dancelor_client_html.NewAPI
module M = Dancelor_client_model

let title_and_subtitle ?(link=true) book =
  let title_text = [L.txt (M.Book.title book)] in
  if link then
    let%lwt subtitle_block =
      match%lwt M.Book.subtitle book with
      | "" -> Lwt.return_nil
      | subtitle -> Lwt.return [
          span ~a:[a_class ["details"]] [txt subtitle]
        ]
    in
    Lwt.return (title_text @ subtitle_block)
  else
    Lwt.return title_text

let short_title ?(link=true) book =
  let short_title_text = [L.txt (M.Book.short_title book)] in
  if link then
    let href =
      let%lwt slug = M.Book.slug book in
      Lwt.return PageRouter.(path (Book slug))
    in
    Lwt.return [a ~a:[L.a_href href] short_title_text]
  else
    Lwt.return short_title_text
