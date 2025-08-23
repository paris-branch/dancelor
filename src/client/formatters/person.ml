open Nes
open Common

open Html

let name_gen person =
  span [
    match person with
    | Right (person, true) ->
      a
        ~a: [a_href @@ Endpoints.Page.href_person @@ Entry.id person]
        [txt (NEString.to_string @@ Model.Person.name' person)]
    | Right (person, _) -> txt (NEString.to_string @@ Model.Person.name' person)
    | Left person -> txt (NEString.to_string @@ Model.Person.name person)
  ]

let name = name_gen % Either.left

let name' ?(link = true) person = name_gen @@ Right (person, link)

let names_gen ?(short = false) persons =
  let persons = List.map (List.singleton % name_gen) persons in
  let components =
    if short then
      match persons with
      | [] -> []
      | [p] -> [p]
      | [p; q] -> [p; [txt " & "]; q]
      | p :: _ -> [p; [txt " et al."]]
    else
      List.interspersei (fun _ -> [txt ", "]) ~last: (fun _ -> [txt " and "]) persons
  in
  span @@ List.concat components

let names ?short = names_gen ?short % List.map Either.left

let names' ?short ?(links = true) persons =
  names_gen ?short @@ List.map (fun person -> Right (person, links)) persons
