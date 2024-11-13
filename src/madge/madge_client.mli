open Madge_common

type add_arg = {a: 'a. ('a, mandatory) arg -> 'a -> unit}
type add_opt_arg = {o: 'a. ('a, optional) arg -> 'a option -> unit}

val call :
  endpoint: 'a endpoint ->
  (add_arg -> add_opt_arg -> unit) ->
  'a Lwt.t
