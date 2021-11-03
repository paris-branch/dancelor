type serialised = Yojson.Safe.t
type 'a serialiser = 'a -> serialised
type 'a unserialiser = serialised -> ('a, string) result

module type SERIALISABLE = sig
  type t

  val _key : string

  val to_yojson : t serialiser
  val of_yojson : t unserialiser
end

type ('a, 'optional) arg = (module SERIALISABLE with type t = 'a)

type mandatory
type optional

(* FIXME: check either through typing or dynamically that optional arguments
   are indeed used optionally. *)

let arg (type s) ?key (module M : SERIALISABLE with type t = s) : (s, mandatory) arg =
  (module struct
    type t = M.t
    let _key = match key with Some key -> key | None -> M._key
    let to_yojson = M.to_yojson
    let of_yojson = M.of_yojson
  end)

let optarg (type s) ?key (module M : SERIALISABLE with type t = s) : (s, optional) arg =
  (module struct
    type t = M.t
    let _key = match key with Some key -> key | None -> M._key
    let to_yojson = M.to_yojson
    let of_yojson = M.of_yojson
  end)

let arg_key (type s) (module M : SERIALISABLE with type t = s) = M._key
let arg_serialiser (type s) (module M : SERIALISABLE with type t = s) : s serialiser = M.to_yojson
let arg_unserialiser (type s) (module M : SERIALISABLE with type t = s) : s unserialiser = M.of_yojson

type 'a endpoint =
  { meth : Cohttp.Code.meth ;
    path : string ;
    returns : (module SERIALISABLE with type t = 'a) }

let endpoint ?(meth=`POST) ~path returns =
  { meth; path; returns }

let endpoint_meth endpoint = endpoint.meth
let endpoint_path endpoint = endpoint.path

let endpoint_serialiser (type s) endpoint =
  let (module M : SERIALISABLE with type t = s) = endpoint.returns in
  M.to_yojson

let endpoint_unserialiser (type s) endpoint =
  let (module M : SERIALISABLE with type t = s) = endpoint.returns in
  M.of_yojson

exception BadQuery of string
let bad_query string = raise (BadQuery string)

let prefix = ref "/madge"

module MUnit : SERIALISABLE
  with type t = unit =
struct
  type t = unit [@@deriving yojson]
  let _key = "unit"
end

module MInteger : SERIALISABLE
  with type t = int =
struct
  type t = int [@@deriving yojson]
  let _key = "int"
end

module MFloat : SERIALISABLE
  with type t = float =
struct
  type t = float [@@deriving yojson]
  let _key = "float"
end

module MString : SERIALISABLE
  with type t = string =
struct
  type t = string [@@deriving yojson]
  let _key = "string"
end

module MOption (A : SERIALISABLE) : SERIALISABLE
  with type t = A.t option =
struct
  type t = A.t option [@@deriving yojson]
  let _key = A._key ^ "-option"
end

module MPair (A : SERIALISABLE) (B : SERIALISABLE) : SERIALISABLE
  with type t = A.t * B.t =
struct
  type t = A.t * B.t [@@deriving yojson]
  let _key = A._key ^ "-" ^ B._key ^ "-pair"
end

module MList (A : SERIALISABLE) : SERIALISABLE
  with type t = A.t list =
struct
  type t = A.t list [@@deriving yojson]
  let _key = A._key ^ "-list"
end

module MSlug (A : SERIALISABLE) : SERIALISABLE
  with type t = A.t NesSlug.t =
struct
  type t = A.t NesSlug.t [@@deriving yojson]
  let _key = A._key ^ "-slug"
end
