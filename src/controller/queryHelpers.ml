open Dancelor_common
open ResultMonad

let query_string q k =
  match List.assoc_opt k q with
  | Some [v] -> Ok v
  | Some (_ :: _) -> Error ("too many arguments for " ^ k)
  | _ -> Error ("an argument is required for " ^ k)

let query_int q k =
  query_string q k >>= fun i ->
  match int_of_string_opt i with
  | Some i -> Ok i
  | None -> Error ("wrong type for " ^ k ^ " (int expected)")
