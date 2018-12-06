open Dancelor_common

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

let query_strings_opt query key = (*FIXME*)
  Some (
      query
      |> List.find_all (fun (k, _) -> key = k)
      |> List.split |> snd
      |> List.map
           (function
            | [value] -> value
            | _ -> error ("the key " ^ key ^ " can only have one value FIXME"))
    )

let query_int query key =
  match int_of_string_opt (query_string query key) with
  | Some int -> int
  | None -> error ("wrong type for " ^ key ^ " (int expected)")
