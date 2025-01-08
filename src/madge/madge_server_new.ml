open Nes
include Madge_common_new
open Route

(* NOTE: The [controller] is in a thunk to avoid it being ran halfway as we find
   its last argument. It is actually run at the end when all is green. *)
let rec match_apply : type a r. (a, r Lwt.t, r) t -> (unit -> a) -> string list -> string Lwt.t option = fun route controller path ->
  match route with
  | Return(module R) ->
    (
      match path with
      | [] -> Some (Lwt.map (Yojson.Safe.to_string % R.to_yojson) (controller ()))
      | _ -> None
    )
  | Literal (str, route) ->
    (
      match path with
      | comp :: path when comp = str -> match_apply route controller path
      | _ -> None
    )
  | Variable (rt, route) ->
    (
      match path with
      | comp :: path ->
        (
          match rt.of_string comp with
          | Some comp -> match_apply route (fun () -> controller () comp) path
          | _ -> None
        )
      | _ -> None
    )

let match_apply : type a r. (a, r Lwt.t, r) t -> a -> Uri.t -> string Lwt.t option = fun route controller uri ->
  let path = List.filter ((<>) "") (String.split_on_char '/' (Uri.path uri)) in
  match_apply route (fun () -> controller) path
