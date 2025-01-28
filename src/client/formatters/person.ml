open Nes
open Dancelor_common
open Html

let name ?(link = true) person =
  let name_text = [txt (Model.Person.name person)] in
  if link then
    [
      a
        ~a: [a_href @@ PageRouter.href_person @@ Database.Entry.slug person]
        name_text
    ]
  else
    name_text

let names ?(short = false) ?link persons =
  let persons = List.map (name ?link) persons in
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
  List.concat components
