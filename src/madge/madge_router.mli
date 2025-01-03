(** {1 Madge Router} *)

open Nes

(** {2 Requests} *)

type request = {
  method_: Cohttp.Code.meth;
  path: string;
  query: Madge_query.t
}
(* FIXME: should be made abstract *)

val request_to_uri : request -> Uri.t

(** {2 Routes} *)

type 'resource route

(** {3 Building Routes} *)

val direct : Cohttp.Code.meth -> string -> 'resource -> 'resource route

val with_slug :
  Cohttp.Code.meth ->
  string ->
  ?ext: string ->
  (('a Slug.t -> 'resource) * ('resource -> 'a Slug.t option)) ->
  'resource route

val with_slug_and_query :
  Cohttp.Code.meth ->
  string ->
  ?ext: string ->
  ('a Slug.t -> Madge_query.t -> 'resource) ->
  ('resource -> ('a Slug.t * Madge_query.t) option) ->
  'resource route

val with_query :
  Cohttp.Code.meth ->
  string ->
  (Madge_query.t -> 'resource) ->
  ('resource -> Madge_query.t option) ->
  'resource route

(** {3 Using Routes} *)

val request_to_resource : request -> 'resource route list -> 'resource option

val resource_to_request : 'resource -> 'resource route list -> request

(** {3 Wrapping Routes} *)

(* FIXME: support adding prefixes *)

val wrap_route :
  ?prefix: string ->
  wrap: ('resource -> 'wresource) ->
  unwrap: ('wresource -> 'resource option) ->
  'resource route ->
  'wresource route

val wrap_routes :
  ?prefix: string ->
  wrap: ('resource -> 'wresource) ->
  unwrap: ('wresource -> 'resource option) ->
  'resource route list ->
  'wresource route list
