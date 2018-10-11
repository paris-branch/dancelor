open Cohttp_lwt_unix

let respond_html html =
  Server.respond_string ~status:`OK ~body:html ()

let respond_json json =
  Server.respond_string ~status:`OK ~body:(Ezjsonm.to_string json) ()

let success l =
  `O (("success", `Bool true) :: l)

let error msg =
  `O [
      "success", `Bool false;
      "message", `String msg
    ]

(* helpers with queries *)

exception ArgumentRequired of string
exception TooManyArguments of string
exception ArgumentOfWrongType of string * string

let query_string q k =
  match List.assoc_opt k q with
  | Some [v] -> v
  | Some (_ :: _) -> raise (TooManyArguments k)
  | _ -> raise (ArgumentRequired k)

let query_int q k =
  try query_string q k |> int_of_string
  with Failure _ -> raise (ArgumentOfWrongType ("int", k))
