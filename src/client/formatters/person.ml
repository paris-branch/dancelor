open Nes
open Dancelor_common
open Dancelor_client_html
module M = Dancelor_client_model

let name ?(link=true) person =
  match person with
  | None -> []
  | Some person ->
    let name_text = [txt (M.Person.name person)] in
    if link then
      [
        a
          ~a:[a_href @@ PageRouter.path_person @@ M.Person.slug person]
          name_text
      ]
    else
      name_text

let names ?(short=false) ?link persons =
  let persons = List.map (name ?link % Option.some) persons in
  let components =
    if short then
      match persons with
      | [] -> []
      | [p] -> [p]
      | [p; q] -> [p; [txt " & "]; q]
      | p :: _ -> [p; [txt " et al."]]
    else
      List.interspersei (fun _ -> [txt ", "]) ~last:(fun _ -> [txt " and "]) persons
  in
  List.concat components
