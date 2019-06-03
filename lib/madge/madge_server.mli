open Madge_common

type get_arg     = { a : 'a. 'a arg -> 'a }
type get_opt_arg = { o : 'a. 'a arg -> 'a option }

val register :
  endpoint:'a endpoint ->
  (get_arg -> get_opt_arg -> 'a Lwt.t) ->
  unit

val handle :
  Cohttp.Code.meth -> string ->
  (string * string list) list ->
  (Cohttp.Response.t * Cohttp_lwt.Body.t) option Lwt.t
