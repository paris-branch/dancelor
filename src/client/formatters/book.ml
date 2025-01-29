open Common

open Html

let title_and_subtitle ?(link = true) book =
  let title_text = [txt (Model.Book.title book)] in
  if link then
    let subtitle_block =
      match Model.Book.subtitle book with
      | "" -> []
      | subtitle -> [span ~a: [a_class ["details"]] [txt subtitle]]
    in
    (title_text @ subtitle_block)
  else
    title_text

let short_title ?(link = true) book =
  let short_title_text = [txt (Model.Book.short_title book)] in
  if link then
    [
      a
        ~a: [a_href @@ Endpoints.Page.href_book @@ Entry.slug book]
        short_title_text
    ]
  else
    short_title_text
