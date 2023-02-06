open Madge_common

type get_arg = { a: 'a.('a , mandatory ) arg -> 'a; }
type get_opt_arg = { o: 'a.('a , optional ) arg -> 'a option; }

val register :
  endpoint: 'a endpoint ->
  (get_arg -> get_opt_arg -> 'a Lwt.t) ->
  unit

val handle :
  Cohttp.Code.meth ->
  string ->
  (string * Yojson.Safe.t ) list ->
  (Cohttp.Response.t * Cohttp_lwt.Body.t ) option Lwt.t
