open Nes
open Common

open Html

let title_gen book_gen =
  span @@
    match book_gen with
    | Right (book, true, context) ->
      let title = Model.Book.title' book in
        [a ~a: [a_href @@ Endpoints.Page.href_book ?context @@ Entry.id book] [txt @@ NEString.to_string title]]
    | Right (book, _, _) ->
      let title = Model.Book.title' book in
        [txt @@ NEString.to_string title]
    | Left book ->
      let title = Model.Book.title book in
        [txt @@ NEString.to_string title]

let title' ?(link = true) ?context book = title_gen @@ Right (book, link, context)

let editors book =
  with_span_placeholder @@
    match%lwt Model.Book.authors book with
    | [] -> lwt_nil
    | editors -> lwt [Person.names' ~links: true editors]

let editors' book = editors @@ Entry.value book

let date_and_editors book =
  with_span_placeholder @@ (
    let date =
      match Model.Book.date book with
      | None -> []
      | Some date -> [txt (spf "Published %s" (NesPartialDate.to_pretty_string ~at: true date))]
    in
    let%lwt editors =
      match%lwt Model.Book.authors book with
      | [] -> lwt_nil
      | editors -> lwt [txt "by "; Person.names' ~links: true editors]
    in
    lwt (date @ [txt " "] @ editors)
  )

let date_and_editors' book = date_and_editors @@ Entry.value book
