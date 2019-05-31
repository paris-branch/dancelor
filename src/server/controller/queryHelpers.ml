open Nes
open Dancelor_common.Error

type key = string
type query = (key * string list) list

let bad_query message =
  Lwt.fail (Exn (BadQuery message))

let query_strings_opt query key =
  match List.assoc_opt key query with
  | Some strings -> Lwt.return_some strings
  | None -> Lwt.return_none

let query_strings ?or_ query key =
  match List.assoc_opt key query with
  | Some strings -> Lwt.return strings
  | None ->
    match or_ with
    | None -> bad_query ("the key '" ^ key ^ "' is required")
    | Some or_ -> Lwt.return or_

let query_string_opt query key =
  match List.assoc_opt key query with
  | Some [value] -> Lwt.return_some value
  | Some (_ :: _) -> bad_query ("the key '" ^ key ^ "' can only have one value")
  | _ -> Lwt.return_none

let query_string ?or_ query key =
  match%lwt query_string_opt query key with
  | Some value -> Lwt.return value
  | None ->
    match or_ with
    | None -> bad_query ("the key '" ^ key ^ "' is required")
    | Some or_ -> Lwt.return or_

let query_int_opt query key =
  match%lwt query_string_opt query key with
  | None -> Lwt.return_none
  | Some string ->
    match int_of_string_opt string with
    | Some int -> Lwt.return_some int
    | None -> bad_query ("wrong type for " ^ key ^ " (int expected)")

let query_int ?or_ query key =
  match%lwt query_int_opt query key with
  | Some int -> Lwt.return int
  | None ->
    match or_ with
    | None -> bad_query ("the key '" ^ key ^ "' is required")
    | Some or_ -> Lwt.return or_

let query_float_opt query key =
  match%lwt query_string_opt query key with
  | None -> Lwt.return_none
  | Some string ->
    match float_of_string_opt string with
    | Some float -> Lwt.return_some float
    | None -> bad_query ("wrong type for " ^ key ^ " (float expected)")

let query_float ?or_ query key =
  match%lwt query_float_opt query key with
  | Some float -> Lwt.return float
  | None ->
    match or_ with
    | None -> bad_query ("the key '" ^ key ^ "' is required")
    | Some or_ -> Lwt.return or_
