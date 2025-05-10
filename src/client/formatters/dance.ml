open Nes
open Common

open Html

let name_gen = function
  | Right (dance, true) ->
    a
      ~a: [a_href @@ Endpoints.Page.href_dance @@ Entry.slug dance]
      [txt (Model.Dance.name' dance)]
  | Right (dance, _) -> txt (Model.Dance.name' dance)
  | Left dance -> txt (Model.Dance.name dance)

let name = name_gen % Either.left
let name' ?(link = true) person = name_gen @@ Right (person, link)
