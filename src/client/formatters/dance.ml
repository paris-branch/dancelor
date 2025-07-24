open Nes
open Common

open Html

let name_gen dance_gen =
  span [
    match dance_gen with
    | Right (dance, true) ->
      a
        ~a: [a_href @@ Endpoints.Page.href_dance @@ Entry.id dance]
        [txt (Model.Dance.name' dance)]
    | Right (dance, _) -> txt (Model.Dance.name' dance)
    | Left dance -> txt (Model.Dance.name dance)
  ]

let name = name_gen % Either.left

let name' ?(link = true) person = name_gen @@ Right (person, link)

let name_and_disambiguation_gen dance =
  let disambiguation_block =
    match Model.Dance.disambiguation @@ Either.fold ~left: Fun.id ~right: (Entry.value % fst) dance with
    | "" -> []
    | disambiguation -> [span ~a: [a_class ["opacity-50"]] [txt (spf " (%s)" disambiguation)]]
  in
  span (name_gen dance :: disambiguation_block)

let name_and_disambiguation =
  name_and_disambiguation_gen % Either.left

let name_and_disambiguation' ?(name_link = true) dance =
  name_and_disambiguation_gen @@ Right (dance, name_link)
