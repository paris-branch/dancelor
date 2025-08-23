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
  span (title_text @ subtitle_block)

let title_and_subtitle' = title_and_subtitle % Entry.value

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
