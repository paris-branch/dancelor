open Madge_common

val add_arg : query -> 'a arg -> 'a -> unit
val add_opt_arg : query -> 'a arg -> 'a option -> unit

val call : endpoint:'a endpoint -> (query -> unit) -> 'a Lwt.t
