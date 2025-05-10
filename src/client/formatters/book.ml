open Nes
open Common

open Html

let title_and_subtitle book =
  let title_text = [txt (Model.Book.title book)] in
  let subtitle_block =
    match Model.Book.subtitle book with
    | "" -> []
    | subtitle -> [br (); span ~a: [a_class ["opacity-75"]] [txt subtitle]]
  in
  title_text @ subtitle_block

let title_and_subtitle' = title_and_subtitle % Entry.value

let short_title' ?(link = true) book =
  let short_title_text = [txt (Model.Book.short_title' book)] in
  if link then
    [
      a
        ~a: [a_href @@ Endpoints.Page.href_book @@ Entry.slug book]
        short_title_text
    ]
  else
    short_title_text
