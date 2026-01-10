open Nes
open Common
open Html

let switch_signal_option = function
  | None -> S.Option.none
  | Some signal -> S.Option.some signal

let name_gen person =
  span [
    match person with
    | Right (person, true, context) ->
      a
        ~a: [
          R.a_href @@ S.map (fun context -> Endpoints.Page.href_person ?context @@ Entry.id person) (switch_signal_option context)
        ]
        [txt (NEString.to_string @@ Model.Person.name' person)]
    | Right (person, _, _) -> txt (NEString.to_string @@ Model.Person.name' person)
    | Left person -> txt (NEString.to_string @@ Model.Person.name person)
  ]

let name = name_gen % Either.left

let name' ?(link = true) ?context person = name_gen @@ Right (person, link, context)

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
  names_gen ?short @@ List.map (fun person -> Right (person, links, None)) persons
