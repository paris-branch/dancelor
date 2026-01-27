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

let names_with_details_gen ?(short = false) persons_with_details =
  let persons =
    List.map
      (fun (person, details) -> name_gen person :: details)
      persons_with_details
  in
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

let names_with_details ?short =
  names_with_details_gen ?short % List.map (Pair.map_fst Either.left)

let names'_with_details ?short ?(links = true) persons =
  names_with_details_gen ?short @@ List.map (fun (person, details) -> (Right (person, links, None), details)) persons

let names ?short = names_with_details ?short % List.map (Pair.snoc [])

let names' ?short ?links = names'_with_details ?short ?links % List.map (Pair.snoc [])
