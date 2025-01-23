(** {1 Madge}

    Simple GADT-based routing. *)

open Nes

include Serialisation

type meth =
  | GET

let meth_to_string = function
  | GET -> "GET"

(** Abstract type of a route. The type arguments are (1) the function type
    corresponding to the route, (2) the return value of that function type, (3)
    the return value from the route. *)
type (_, _, _) route =
  | Return : meth * (module JSONABLE with type t = 'r) -> ('w, 'w, 'r) route
  | Literal : string * ('a, 'w, 'r) route -> ('a, 'w, 'r) route
  | Variable : string * (module STRINGABLE with type t = 'a) * string * ('b, 'w, 'r) route -> (('a -> 'b), 'w, 'r) route
  | Query :
      string
      * ('b option -> (('c -> 'a) -> 'a) option) (* proxy *)
      * (('b option -> 'a) -> ('c -> 'a)) (* unproxy *)
      * (module JSONABLE with type t = 'b)
      * ('a, 'w, 'r) route ->
      (('c -> 'a), 'w, 'r) route
  (* FIXME: way to construct body parameters *)

let return meth rt = Return (meth, rt)
let literal str route = Literal (str, route)
let variable ?(prefix = "") ?(suffix = "") rt route = Variable (prefix, rt, suffix, route)

let void = return (module JVoid)
let get rt = return GET rt

let query_opt name rt route =
  let proxy = Option.some % (fun x f -> f x) in
  Query (name, proxy, Fun.id, rt, route)

let query name rt route =
  let proxy = Option.map (fun x f -> f x) in
  let unproxy = fun f x -> f (Some x) in
  Query (name, proxy, unproxy, rt, route)

type request = {
  meth: meth;
  uri: Uri.t;
  body: string;
}

exception IllegalBodyInGetRequest

let rec process
  : type a w r. string ->
    Madge_query.t ->
    Madge_query.t ->
    (a, w, r) route ->
    ((module JSONABLE with type t = r) -> request -> w) ->
    a
  = fun path query body route return ->
    match route with
    | Return (meth, (module R)) ->
      (
        let uri = Uri.make ~path ~query: (Madge_query.to_strings query) () in
        let body =
          if meth = GET then
            (
              if not (Madge_query.is_empty body) then
                raise IllegalBodyInGetRequest;
              ""
            )
          else
            Yojson.Safe.to_string @@ `Assoc (Madge_query.to_list body)
        in
        return (module R) {meth; uri; body}
      )
    | Literal (str, route) ->
      process (path ^ "/" ^ str) query body route return
    | Variable (prefix, (module R), suffix, route) ->
      fun x -> process (path ^ "/" ^ prefix ^ Uri.pct_encode (R.to_string x) ^ suffix) query body route return
    | Query (name, _, unproxy, (module R), route) ->
      (
        unproxy @@ function
        | None ->
          process path query body route return
        | Some x ->
          let query = Madge_query.add name (R.to_yojson x) query in
          process path query body route return
      )

let process
  : type a w r. (a, w, r) route ->
    ((module JSONABLE with type t = r) -> request -> w) ->
    a
  = fun route return ->
    process "" Madge_query.empty Madge_query.empty route return

(* NOTE: The [controller] is in a thunk to avoid it being ran halfway as we find
   its last argument. It is actually run at the end when all is green. *)
let rec match_
  : type a w r z. (a, w, r) route ->
    (unit -> a) ->
    meth ->
    string list ->
    Madge_query.t ->
    Madge_query.t ->
    ((module JSONABLE with type t = r) -> w -> z) ->
    (unit -> z) option
  = fun route controller meth path query body return ->
    match route with
    | Return (meth', (module R)) ->
      (
        if meth' = meth && path = [] && Madge_query.is_empty query && Madge_query.is_empty body then
          Some (fun () -> return (module R) (controller ()))
        else
          None
      )
    | Literal (str, route) ->
      (
        match path with
        | comp :: path when comp = str -> match_ route controller meth path query body return
        | _ -> None
      )
    | Variable (prefix, (module R), suffix, route) ->
      (
        match path with
        | [] -> None
        | comp :: path ->
          Option.bind (String.remove_prefix ~needle: prefix comp) @@ fun comp ->
          Option.bind (String.remove_suffix ~needle: suffix comp) @@ fun comp ->
          Option.bind (R.of_string comp) @@ fun comp ->
          match_ route (fun () -> controller () comp) meth path query body return
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
          | Some f -> match_ route (fun () -> f (controller ())) meth path query body return
      )

let match_
  : type a w r z. (a, w, r) route ->
    a ->
    request ->
    ((module JSONABLE with type t = r) -> w -> z) ->
    (unit -> z) option
  = fun route controller {meth; uri; body} return ->
    let path = List.filter ((<>) "") (String.split_on_char '/' (Uri.path uri)) in
    match_ route (fun () -> controller) meth path (Madge_query.from_uri uri) (Madge_query.from_body body) return

let match_'
  : type a w r. (a, w, r) route ->
    a ->
    request ->
    (unit -> w) option
  = fun route controller request ->
    match_ route controller request (fun _ x -> x)

let uri : type a r. (a, Uri.t, r) route -> a = fun route ->
  process route (fun (module _) {uri; _} -> uri)
