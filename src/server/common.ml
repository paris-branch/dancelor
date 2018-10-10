include Cohttp_lwt_unix
include Dancelor_model

let respond ?(status=`OK) json =
  Server.respond_string ~status ~body:(Ezjsonm.to_string json) ()

let success l =
  `O (("success", `Bool true) :: l)
  |> respond

let error status msg =
  `O [
      "success", `Bool false;
      "message", `String msg
    ]
  |> respond ~status

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
