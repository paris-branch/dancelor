open Common

let get_png query =
  match List.assoc_opt "slug" query with
  | Some [_slug] ->
     error `Bad_request ":-)"
  | _ ->
     error `Bad_request ":-("
