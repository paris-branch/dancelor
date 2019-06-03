open Madge_common

type add_arg     = { a : 'a. 'a arg -> 'a -> unit }
type add_opt_arg = { o : 'a. 'a arg -> 'a option -> unit }

val call :
  endpoint:'a endpoint ->
  (add_arg -> add_opt_arg -> unit) ->
  'a Lwt.t
