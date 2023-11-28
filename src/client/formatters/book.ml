open Dancelor_common
open Dancelor_client_html
module M = Dancelor_client_model

let title_and_subtitle ?(link=true) book =
  let title_text = [txt (M.Book.title book)] in
  if link then
    let subtitle_block =
      match M.Book.subtitle book with
      | "" -> []
      | subtitle ->  [span ~a:[a_class ["details"]] [txt subtitle]]
    in
    (title_text @ subtitle_block)
  else
    title_text

let short_title ?(link=true) book =
  let short_title_text = [txt (M.Book.short_title book)] in
  if link then
    [
      a
        ~a:[a_href @@ PageRouter.path_book @@ M.Book.slug book]
        short_title_text
    ]
  else
    short_title_text
