open Dancelor_common
open Option

let error ?(status=`OK) message =
  raise (Error.Error (status, message))

let query_string query key =
  match List.assoc_opt key query with
  | Some [value] -> value
  | Some (_ :: _) -> error ("the key '" ^ key ^ "' can only have one value")
  | _ -> error ("the key '" ^ key ^ "' is required")

let query_string_opt query key =
  catch_and_wrap (fun () -> query_string query key)

let query_string_or query key or_ =
  try query_string query key
  with _ -> or_

let query_strings_opt query key =
  List.assoc_opt key query

let query_strings query key =
  match query_strings_opt query key with
  | Some strings -> strings
  | None -> error ("the key '" ^ key ^ "' is required")

let query_strings_or query key or_ =
  try query_strings query key
  with _ -> or_

let query_int query key =
  match int_of_string_opt (query_string query key) with
  | Some int -> int
  | None -> error ("wrong type for " ^ key ^ " (int expected)")

let query_int_opt query key =
  query_string_opt query key >>=
    int_of_string_opt

let query_float_opt query key =
  query_string_opt query key >>=
    float_of_string_opt
