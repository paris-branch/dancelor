open Madge_common

val get_arg : query -> 'a arg -> 'a
val get_opt_arg : query -> 'a arg -> 'a option

val register : endpoint:'a endpoint -> (query -> 'a Lwt.t) -> unit

val handle :
  Cohttp.Code.meth -> string ->
  (string * string list) list ->
  (Cohttp.Response.t * Cohttp_lwt.Body.t) option Lwt.t
