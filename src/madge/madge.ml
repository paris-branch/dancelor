(** {1 Madge}

    Simple GADT-based routing. *)

open Nes

module type STRINGABLE = sig
  type t
  val to_string : t -> string
  val of_string : string -> t option
end

module type JSONABLE = sig
  type t
  val to_yojson : t -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> (t, string) result
end

module SString : STRINGABLE with type t = string = struct
  type t = string
  let to_string = Fun.id
  let of_string = Option.some
end

module JVoid : JSONABLE with type t = Void.t = struct
  type t = Void.t [@@deriving yojson]
end

(* For the slugs, we can stringify them just like strings, no matter what type
   they carry. *)
module type TYPEABLE = sig type t end
module SSlug (A : TYPEABLE) : STRINGABLE with type t = A.t Slug.t = struct
  type t = A.t Slug.t
  let to_string = Slug.to_string
  let of_string = Option.some % Slug.unsafe_of_string
end

(** Abstract type of a route. The type arguments are (1) the function type
    corresponding to the route, (2) the return value of that function type, (3)
    the return value from the route. *)
type (_, _, _) route =
  | Return : (module JSONABLE with type t = 'r) -> ('w, 'w, 'r) route
  | Literal : string * ('a, 'w, 'r) route -> ('a, 'w, 'r) route
  | Variable : (module STRINGABLE with type t = 'a) * ('b, 'w, 'r) route -> (('a -> 'b), 'w, 'r) route
  | Query : string * (module JSONABLE with type t = 'a) * ('b, 'w, 'r) route -> (('a -> 'b), 'w, 'r) route
  | QueryOpt : string * (module JSONABLE with type t = 'a) * ('b, 'w, 'r) route -> (('a option -> 'b), 'w, 'r) route

let return rt = Return rt
let literal str route = Literal (str, route)
let variable rt route = Variable (rt, route)
let query name rt route = Query (name, rt, route)
let query_opt name rt route = QueryOpt (name, rt, route)

let rec process
  : type a w r. Buffer.t ->
    Madge_query.t ->
    (a, w, r) route ->
    ((module JSONABLE with type t = r) -> Uri.t -> w) ->
    a
  = fun path query route return ->
    match route with
    | Return(module R) ->
      let uri = Uri.make ~path: (Buffer.contents path) ~query: (Madge_query.to_strings query) () in
      return (module R) uri
    | Literal (str, route) ->
      Buffer.add_char path '/';
      Buffer.add_string path str;
      process path query route return
    | Variable ((module R), route) ->
      fun x ->
        Buffer.add_char path '/';
        (* FIXME: url encode *)
        Buffer.add_string path (R.to_string x);
        process path query route return
    | Query (name, (module R), route) ->
      fun x ->
        let query = Madge_query.add name (R.to_yojson x) query in
        process path query route return
    | QueryOpt (name, (module R), route) ->
      function
      | None ->
        process path query route return
      | Some x ->
        let query = Madge_query.add name (R.to_yojson x) query in
        process path query route return

let process
  : type a w r. (a, w, r) route ->
    ((module JSONABLE with type t = r) -> Uri.t -> w) ->
    a
  = fun route return ->
    process (Buffer.create 8) Madge_query.empty route return

(* NOTE: The [controller] is in a thunk to avoid it being ran halfway as we find
   its last argument. It is actually run at the end when all is green. *)
let rec match_
  : type a w r z. (a, w, r) route ->
    (unit -> a) ->
    string list ->
    Madge_query.t ->
    ((module JSONABLE with type t = r) -> w -> z) ->
    (unit -> z) option
  = fun route controller path query return ->
    match route with
    | Return(module R) ->
      (
        if path = [] && Madge_query.is_empty query then
          Some (fun () -> return (module R) (controller ()))
        else
          None
      )
    | Literal (str, route) ->
      (
        match path with
        | comp :: path when comp = str -> match_ route controller path query return
        | _ -> None
      )
    | Variable ((module R), route) ->
      (
        match path with
        | comp :: path ->
          (
            match R.of_string comp with
            | Some comp -> match_ route (fun () -> controller () comp) path query return
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
            | Ok value -> match_ route (fun () -> controller () value) path query return
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
            | Ok value -> match_ route (fun () -> controller () (Some value)) path query return
            | _ -> None
          )
        | None -> match_ route (fun () -> controller () None) path query return
      )

let match_
  : type a w r z. (a, w, r) route ->
    a ->
    Uri.t ->
    ((module JSONABLE with type t = r) -> w -> z) ->
    (unit -> z) option
  = fun route controller uri return ->
    let path = List.filter ((<>) "") (String.split_on_char '/' (Uri.path uri)) in
    match_ route (fun () -> controller) path (Madge_query.from_uri uri) return

let uri : type a r. (a, Uri.t, r) route -> a = fun route ->
  process route (fun (module _) uri -> uri)
