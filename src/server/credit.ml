open Cohttp_lwt_unix
module Database = Dancelor_database
open Dancelor_model
             
let respond ?(status=`OK) json =
  Server.respond_string ~status ~body:(Ezjsonm.to_string json) ()

let success = function
  | `O l -> `O (("success", `Bool true) :: l) |> respond
  | _ -> assert false

let error status msg =
  `O [
      "success", `Bool false;
      "message", `String msg
    ]
  |> respond ~status


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
