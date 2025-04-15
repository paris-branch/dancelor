(** {1 Madge}

    Simple GADT-based routing. *)

open Nes
open Serialisation

type ('a, 'w, 'r) route
(** Abstract type of a route. The type arguments are (1) the function type
    corresponding to the route, (2) the return value of that function type, (3)
    the return value from the route. For instance, if the route corresponds to a
    function [int -> float -> bool], then it will be a [(int -> float -> 'w, 'w,
    bool) route]. *)

(** {2 Constructing routes}

    For instance: [literal "api" @@ variable (module SString) @@ query
    "threshold" (module JFloat) @@ get (module JInt)]. *)

val literal :
  string ->
  ('a, 'w, 'r) route ->
  ('a, 'w, 'r) route
(** Takes a route and prefixes it by a component that is a constant string
    literal. *)

val variable :
  ?prefix: string ->
  ?suffix: string ->
  (module STRINGABLE with type t = 'a) ->
  ('b, 'w, 'r) route ->
  (('a -> 'b), 'w, 'r) route

val query :
  string ->
  (module JSONABLE with type t = 'b) ->
  ('a, 'w, 'r) route ->
  (('b -> 'a), 'w, 'r) route

val query_opt :
  string ->
  (module JSONABLE with type t = 'b) ->
  ('a, 'w, 'r) route ->
  (('b option -> 'a), 'w, 'r) route

val body :
  string ->
  (module JSONABLE with type t = 'b) ->
  ('a, 'w, 'r) route ->
  (('b -> 'a), 'w, 'r) route

val body_opt :
  string ->
  (module JSONABLE with type t = 'b) ->
  ('a, 'w, 'r) route ->
  (('b option -> 'a), 'w, 'r) route

val void : unit -> ('w, 'w, Void.t) route
(** Route that returns nothing usable for Madge. This is useful in particular
    for routes that only serve to resolve to a URI, or routes that return a
    document. *)

val get : (module JSONABLE with type t = 'r) -> ('w, 'w, 'r) route
val post : (module JSONABLE with type t = 'r) -> ('w, 'w, 'r) route
val head : (module JSONABLE with type t = 'r) -> ('w, 'w, 'r) route
val delete : (module JSONABLE with type t = 'r) -> ('w, 'w, 'r) route
val patch : (module JSONABLE with type t = 'r) -> ('w, 'w, 'r) route
val put : (module JSONABLE with type t = 'r) -> ('w, 'w, 'r) route
val options : (module JSONABLE with type t = 'r) -> ('w, 'w, 'r) route
val trace : (module JSONABLE with type t = 'r) -> ('w, 'w, 'r) route
val connect : (module JSONABLE with type t = 'r) -> ('w, 'w, 'r) route
(** Basic routes matching on the method and returning a value. *)

(** {2 Using routes} *)

type meth = GET | POST | HEAD | DELETE | PATCH | PUT | OPTIONS | TRACE | CONNECT

val meth_to_string : meth -> string

type request = {
  meth: meth;
  uri: Uri.t;
  body: string;
}
(** Madge's representation of an HTTP request. *)

(** {3 from routes} *)

val uri : ('a, Uri.t, 'r) route -> 'a
(** Easiest way to use a route: transform it into a URI. The ['a] parameter
    corresponds to the function of that route, with [Uri.t] as return type. For
    instance, if [r] has type [(int -> float -> 'w, 'w, 'r) route], then [uri r]
    will have type [int -> float -> Uri.t]. *)

val process :
  ('a, 'w, 'r) route ->
  ((module JSONABLE with type t = 'r) -> request -> 'w) ->
  'a
(** Generic way to use a route. Given a route and a continuation, create a
    request from the route, pass that request to the continuation, and return
    the resulting value. For instance, {!uri} is simply [process route (fun
    (module _) {uri; _} -> uri)]. *)
(* FIXME: better name *)

(** {3 to routes} *)

val match_' :
  ('a, 'w, 'r) route ->
  'a ->
  request ->
  (unit -> 'w) option
(** Given a route, a controller, and a request, check whether the route matches
    the request. If so, construct a thunk that calls the controller with the
    correct arguments. Otherwise, return [None]. *)
(* FIXME: better name *)

val match_ :
  ('a, 'w, 'r) route ->
  'a ->
  request ->
  ((module JSONABLE with type t = 'r) -> 'w -> 'z) ->
  (unit -> 'z) option
(** Given a route, a controller, a request, and a continuation, check whether
    the route matches. If so, construct a thunk that calls the controller with
    the correct arguments and passes its return value to the continuation.
    Otherwise, return [None]. For instance, {!match_'} is simply [match_ route
    controller request (fun _ x -> x)]. *)
(* FIXME: better name *)

(** {2 Serialisation}

    Constructing routes involves manipulating values in a serialisable form,
    either as a string or as json. We provide here some serialisation helpers
    for standard types. *)

include module type of Serialisation
