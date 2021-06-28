open Nes
include Dancelor_common_model.CreditFilter

let person = person >=>| Lwt_list.map_s Person.get

let only_person = Person.slug >=>| only_person

let add_person person filter =
  let%lwt person = Person.slug person in
  add_person person filter

let remove_person person filter =
  let%lwt person = Person.slug person in
  remove_person person filter
