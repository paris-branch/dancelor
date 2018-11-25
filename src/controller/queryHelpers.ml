open Dancelor_common

let query_string q k =
  match List.assoc_opt k q with
  | Some [v] -> v
  | Some (_ :: _) -> raise (Error.Error (`OK, "too many arguments for " ^ k))
  | _ -> raise (Error.Error (`OK, "an argument is required for " ^ k))

let query_int q k =
  match int_of_string_opt (query_string q k) with
  | Some i -> i
  | None -> raise (Error.Error (`OK, "wrong type for " ^ k ^ " (int expected)"))
