type serialised = Yojson.Safe.t
type 'a serialiser = 'a -> serialised
type 'a unserialiser = serialised -> ('a, string) result

module type SERIALISABLE = sig
  type t

  val _key : string

  val to_yojson : t serialiser
  val of_yojson : t unserialiser
end

type ('a, 'optional) arg

type mandatory
type optional

val arg : ?key: string -> (module SERIALISABLE with type t = 'a) -> ('a, mandatory) arg
val optarg : ?key: string -> (module SERIALISABLE with type t = 'a) -> ('a, optional) arg

val arg_key : ('a, 'optional) arg -> string
val arg_serialiser : ('a, 'optional) arg -> 'a serialiser
val arg_unserialiser : ('a, 'optional) arg -> 'a unserialiser

type 'a endpoint

val endpoint :
  ?meth: Cohttp.Code.meth ->
  path: string ->
  (module SERIALISABLE with type t = 'a) ->
  'a endpoint

val endpoint_meth : 'a endpoint -> Cohttp.Code.meth
val endpoint_path : 'a endpoint -> string
val endpoint_serialiser : 'a endpoint -> 'a serialiser
val endpoint_unserialiser : 'a endpoint -> 'a unserialiser

exception BadQuery of string
val bad_query : string -> 'a

val prefix : string ref

module MUnit : SERIALISABLE with
  type t = unit

module MBool : SERIALISABLE with
  type t = bool

module MFloat : SERIALISABLE with
  type t = float

module MInteger : SERIALISABLE with
  type t = int

module MString : SERIALISABLE with
  type t = string

module MOption :
  functor (A : SERIALISABLE) ->
  SERIALISABLE with
  type t = A.t option

module MPair :
  functor (A : SERIALISABLE) ->
  functor (B : SERIALISABLE) ->
  SERIALISABLE with
  type t = A.t * B.t

module MTriple :
  functor (A : SERIALISABLE) ->
  functor (B : SERIALISABLE) ->
  functor (C : SERIALISABLE) ->
  SERIALISABLE with
  type t = A.t * B.t * C.t

module MQuadruple :
  functor (A : SERIALISABLE) ->
  functor (B : SERIALISABLE) ->
  functor (C : SERIALISABLE) ->
  functor (D : SERIALISABLE) ->
  SERIALISABLE with
  type t = A.t * B.t * C.t * D.t

module MList :
  functor (A : SERIALISABLE) ->
  SERIALISABLE with
  type t = A.t list

module MSlug :
  functor (A : SERIALISABLE) ->
  SERIALISABLE with
  type t = A.t NesSlug.t
