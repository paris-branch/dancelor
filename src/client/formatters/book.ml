open Nes
open Dancelor_common
open Html

let switch_signal_option = function
  | None -> S.Option.none
  | Some signal -> S.Option.some signal

let name_gen book_gen =
  span @@
    match book_gen with
    | Right (book, true, in_search) ->
      let name = Model.Book.name' book in
      [
        a
          ~a: [
            R.a_href @@ S.map (fun in_search -> Endpoints.Page.href_book ?in_search @@ Entry.id book) (switch_signal_option in_search)
          ]
          [txt @@ NEString.to_string name]
      ]
    | Right (book, _, _) ->
      let name = Model.Book.name' book in
        [txt @@ NEString.to_string name]
    | Left book ->
      let name = Model.Book.name book in
        [txt @@ NEString.to_string name]

let name' ?(link = true) ?in_search book = name_gen @@ Right (book, link, in_search)

let editors book =
  with_span_placeholder @@
    match Model.Book.authors book with
    | [] -> lwt_nil
    | editors ->
      let%lwt editors = Lwt_list.map_p (Option.get <%> Model.Person.get) editors in
      lwt [Person.names' ~links: true editors]

let editors' book = editors @@ Entry.value book

let date_and_editors book =
  with_span_placeholder @@ (
    let date =
      match Model.Book.date book with
      | None -> []
      | Some date -> [txt (spf "Published %s" (NesPartialDate.to_pretty_string ~at: true date))]
    in
    let%lwt editors =
      match Model.Book.authors book with
      | [] -> lwt_nil
      | editors ->
        let%lwt editors = Lwt_list.map_p (Option.get <%> Model.Person.get) editors in
        lwt [txt "by "; Person.names' ~links: true editors]
    in
    lwt (date @ [txt " "] @ editors)
  )

let date_and_editors' book = date_and_editors @@ Entry.value book
