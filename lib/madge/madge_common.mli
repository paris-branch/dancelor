type serialised = Yojson.Safe.t
type 'a serialiser = 'a -> serialised
type 'a unserialiser = serialised -> ('a, string) result

type 'a arg

val arg :
  key:string ->
  serialiser:'a serialiser -> unserialiser:'a unserialiser ->
  'a arg

val optarg :
  key:string ->
  serialiser:'a serialiser -> unserialiser:'a unserialiser ->
  'a arg

val arg_key : 'a arg -> string
val arg_serialiser : 'a arg -> 'a serialiser
val arg_unserialiser : 'a arg -> 'a unserialiser

type 'a endpoint

val endpoint :
  meth:Cohttp.Code.meth ->
  path:string ->
  serialiser:'a serialiser -> unserialiser:'a unserialiser ->
  'a endpoint

val endpoint_meth : 'a endpoint -> Cohttp.Code.meth
val endpoint_path : 'a endpoint -> string
val endpoint_serialiser : 'a endpoint -> 'a serialiser
val endpoint_unserialiser : 'a endpoint -> 'a unserialiser

type query = (string * string list) list ref

exception BadQuery of string
val bad_query : string -> 'a

val unit_to_yojson : unit serialiser
val unit_of_yojson : unit unserialiser

val float_to_yojson : float serialiser
val float_of_yojson : float unserialiser

val string_to_yojson : string serialiser
val string_of_yojson : string unserialiser

val option_to_yojson : 'a serialiser -> 'a option serialiser
val option_of_yojson : 'a unserialiser -> 'a option unserialiser

val list_to_yojson : 'a serialiser -> 'a list serialiser
val list_of_yojson : 'a unserialiser -> 'a list unserialiser

val prefix : string ref
