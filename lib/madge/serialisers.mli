type serialised = Yojson.Safe.t
type 'a serialiser = 'a -> serialised
type 'a unserialiser = serialised -> ('a, string) result

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
