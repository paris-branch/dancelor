include (module type of Serialisers)

type 'a arg

val arg :
  key:string ->
  serialiser:'a serialiser -> unserialiser:'a unserialiser ->
  'a arg

val optarg :
  key:string ->
  serialiser:'a serialiser -> unserialiser:'a unserialiser ->
  'a arg

type 'a endpoint

val endpoint :
  meth:Cohttp.Code.meth ->
  path:string ->
  serialiser:'a serialiser -> unserialiser:'a unserialiser ->
  'a endpoint

type query

val get_arg : query -> 'a arg -> 'a
val get_opt_arg : query -> 'a arg -> 'a option

val register : endpoint:'a endpoint -> (query -> 'a Lwt.t) -> unit

val add_arg : query -> 'a arg -> 'a -> unit
val add_opt_arg : query -> 'a arg -> 'a option -> unit

val call : endpoint:'a endpoint -> (query -> unit) -> 'a Lwt.t
