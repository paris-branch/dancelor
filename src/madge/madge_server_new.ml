open Nes
include Madge_common_new
open Route

(* NOTE: The [controller] is in a thunk to avoid it being ran halfway as we find
   its last argument. It is actually run at the end when all is green. *)
let rec match_apply : type a r. (a, r Lwt.t, r) t -> (unit -> a) -> string list -> Madge_query.t -> string Lwt.t option = fun route controller path query ->
  match route with
  | Return(module R) ->
    (
      if path = [] && Madge_query.is_empty query then
        Some (Lwt.map (Yojson.Safe.to_string % R.to_yojson) (controller ()))
      else
        None
    )
  | Literal (str, route) ->
    (
      match path with
      | comp :: path when comp = str -> match_apply route controller path query
      | _ -> None
    )
  | Variable (rt, route) ->
    (
      match path with
      | comp :: path ->
        (
          match rt.of_string comp with
          | Some comp -> match_apply route (fun () -> controller () comp) path query
          | _ -> None
        )
      | _ -> None
    )
  | Query (name, (module R), route) ->
    (
      match Madge_query.extract name query with
      | Some (value, query) ->
        (
          match R.of_yojson value with
          | Ok value -> match_apply route (fun () -> controller () value) path query
          | _ -> None
        )
      | None -> None
    )
  | QueryOpt (name, (module R), route) ->
    (
      match Madge_query.extract name query with
      | Some (value, query) ->
        (
          match R.of_yojson value with
          | Ok value -> match_apply route (fun () -> controller () (Some value)) path query
          | _ -> None
        )
      | None -> match_apply route (fun () -> controller () None) path query
    )

let match_apply : type a r. (a, r Lwt.t, r) t -> a -> Uri.t -> string Lwt.t option = fun route controller uri ->
  let path = List.filter ((<>) "") (String.split_on_char '/' (Uri.path uri)) in
  match_apply route (fun () -> controller) path (Madge_query.from_uri uri)
