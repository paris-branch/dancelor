(** {1 Route} *)

open Nes
open Serialisation

type ('a, 'w, 'r) t = ('a, 'w, 'r) Route_internal.t
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
  ('a, 'w, 'r) t ->
  ('a, 'w, 'r) t
(** Takes a route and prefixes it by a component that is a constant string
    literal. *)

val variable :
  ?prefix: string ->
  ?suffix: string ->
  (module STRINGABLE with type t = 'a) ->
  ('b, 'w, 'r) t ->
  (('a -> 'b), 'w, 'r) t

val query :
  string ->
  (module JSONABLE with type t = 'b) ->
  ('a, 'w, 'r) t ->
  (('b -> 'a), 'w, 'r) t

val query_opt :
  string ->
  (module JSONABLE with type t = 'b) ->
  ('a, 'w, 'r) t ->
  (('b option -> 'a), 'w, 'r) t

val body :
  string ->
  (module JSONABLE with type t = 'b) ->
  ('a, 'w, 'r) t ->
  (('b -> 'a), 'w, 'r) t

val body_opt :
  string ->
  (module JSONABLE with type t = 'b) ->
  ('a, 'w, 'r) t ->
  (('b option -> 'a), 'w, 'r) t

val void : unit -> ('w, 'w, Void.t) t
(** Route that returns nothing usable for Madge. This is useful in particular
    for routes that only serve to resolve to a URI, or routes that return a
    document. *)

val get : (module JSONABLE with type t = 'r) -> ('w, 'w, 'r) t
val post : (module JSONABLE with type t = 'r) -> ('w, 'w, 'r) t
val head : (module JSONABLE with type t = 'r) -> ('w, 'w, 'r) t
val delete : (module JSONABLE with type t = 'r) -> ('w, 'w, 'r) t
val patch : (module JSONABLE with type t = 'r) -> ('w, 'w, 'r) t
val put : (module JSONABLE with type t = 'r) -> ('w, 'w, 'r) t
val options : (module JSONABLE with type t = 'r) -> ('w, 'w, 'r) t
val trace : (module JSONABLE with type t = 'r) -> ('w, 'w, 'r) t
val connect : (module JSONABLE with type t = 'r) -> ('w, 'w, 'r) t
(** Basic routes matching on the method and returning a value. *)
