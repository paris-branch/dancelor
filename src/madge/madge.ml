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
module JSlug (A : TYPEABLE) : JSONABLE with type t = A.t Slug.t = struct
  type t = A.t Slug.t
  let to_yojson x = `String (Slug.to_string x)
  let of_yojson = function `String s -> Ok (Slug.unsafe_of_string s) | _ -> Error "JSlug.of_yojson"
end

(** Abstract type of a route. The type arguments are (1) the function type
    corresponding to the route, (2) the return value of that function type, (3)
    the return value from the route. *)
type (_, _, _) route =
  | Return : (module JSONABLE with type t = 'r) -> ('w, 'w, 'r) route
  | Literal : string * ('a, 'w, 'r) route -> ('a, 'w, 'r) route
  | Variable : (module STRINGABLE with type t = 'a) * ('b, 'w, 'r) route -> (('a -> 'b), 'w, 'r) route
  | Query :
      string
      * ('b option -> (('c -> 'a) -> 'a) option) (* proxy *)
      * (('b option -> 'a) -> ('c -> 'a)) (* unproxy *)
      * (module JSONABLE with type t = 'b)
      * ('a, 'w, 'r) route ->
      (('c -> 'a), 'w, 'r) route

let return rt = Return rt
let literal str route = Literal (str, route)
let variable rt route = Variable (rt, route)

let query_opt name rt route =
  let proxy = Option.some % (fun x f -> f x) in
  Query (name, proxy, Fun.id, rt, route)

let query name rt route =
  let proxy = Option.map (fun x f -> f x) in
  let unproxy = fun f x -> f (Some x) in
  Query (name, proxy, unproxy, rt, route)

let rec process
  : type a w r. string ->
    Madge_query.t ->
    (a, w, r) route ->
    ((module JSONABLE with type t = r) -> Uri.t -> w) ->
    a
  = fun path query route return ->
    match route with
    | Return(module R) ->
      let uri = Uri.make ~path ~query: (Madge_query.to_strings query) () in
      return (module R) uri
    | Literal (str, route) ->
      process (path ^ "/" ^ str) query route return
    | Variable ((module R), route) ->
      fun x ->
        (* FIXME: url encode *)
        process (path ^ "/" ^ R.to_string x) query route return
    | Query (name, _, unproxy, (module R), route) ->
      (
        unproxy @@ function
        | None ->
          process path query route return
        | Some x ->
          let query = Madge_query.add name (R.to_yojson x) query in
          process path query route return
      )

let process
  : type a w r. (a, w, r) route ->
    ((module JSONABLE with type t = r) -> Uri.t -> w) ->
    a
  = fun route return ->
    process "" Madge_query.empty route return

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
    | Query (name, proxy, _, (module R), route) ->
      (
        let extract_and_parse =
          match Madge_query.extract name query with
          | None -> Ok (None, query) (* absent: OK *)
          | Some (value, query) ->
            match R.of_yojson value with
            | Ok value -> Ok (Some value, query)
            | Error _ -> Error "unparseable" (* present but unparseable: error *)
        in
        match extract_and_parse with
        | Error _ -> None (* unparseable: the route does not match *)
        | Ok (maybe_value, query) ->
          match proxy maybe_value with
          | None -> None
          | Some f -> match_ route (fun () -> f (controller ())) path query return
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

let match_'
  : type a w r. (a, w, r) route ->
    a ->
    Uri.t ->
    (unit -> w) option
  = fun route controller uri ->
    match_ route controller uri (fun _ x -> x)

let uri : type a r. (a, Uri.t, r) route -> a = fun route ->
  process route (fun (module _) uri -> uri)
