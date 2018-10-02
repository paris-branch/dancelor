open Common


let get query =
  match List.assoc_opt "slug" query with
  | Some [slug] ->
     let credit = Database.Credit.get slug in
     `O [ ("credit", `String (Credit.credit credit)) ;
          ("persons", `A (Credit.persons credit
                          |> List.map (fun person -> `String (Person.name person)))) ]
     |> respond

  | _ ->
     error `Bad_request ":-("
