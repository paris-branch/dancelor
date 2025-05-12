open Ppx_yojson_conv_lib.Yojson_conv
open Nes

module type STRINGABLE = sig
  type t
  val to_string : t -> string
  val of_string : string -> t option
end

module type JSONABLE = Ppx_yojson_conv_lib.Yojsonable.S

(* Base *)

module SString : STRINGABLE with type t = string = struct
  type t = string
  let to_string = Fun.id
  let of_string = Option.some
end

module SStatusCode : STRINGABLE with type t = Cohttp.Code.status_code = struct
  type t = Cohttp.Code.status_code
  let to_string = string_of_int % Cohttp.Code.code_of_status
  let of_string = Option.map Cohttp.Code.status_of_code % int_of_string_opt
end

module JString : JSONABLE with type t = string = struct
  type t = string [@@deriving yojson]
end

module JUri : JSONABLE with type t = Uri.t = struct
  type t = Uri.t
  let yojson_of_t = JString.yojson_of_t % Uri.to_string
  let t_of_yojson = Uri.of_string % JString.t_of_yojson
end

module JUnit : JSONABLE with type t = unit = struct
  type t = unit [@@deriving yojson]
end

module JVoid : JSONABLE with type t = Void.t = struct
  type t = Void.t [@@deriving yojson]
end

module JBool : JSONABLE with type t = bool = struct
  type t = bool [@@deriving yojson]
end

module JInt : JSONABLE with type t = int = struct
  type t = int [@@deriving yojson]
end

module JFloat : JSONABLE with type t = float = struct
  type t = float [@@deriving yojson]
end

(* Higher order *)

module JOption (A : JSONABLE) : JSONABLE with type t = A.t option = struct
  type t = A.t option [@@deriving yojson]
end

module JList (A : JSONABLE) : JSONABLE with type t = A.t list = struct
  type t = A.t list [@@deriving yojson]
end

module JPair (A : JSONABLE) (B : JSONABLE) : JSONABLE with type t = A.t * B.t = struct
  type t = A.t * B.t [@@deriving yojson]
end

module JTriple (A : JSONABLE) (B : JSONABLE) (C : JSONABLE) : JSONABLE with type t = A.t * B.t * C.t = struct
  type t = A.t * B.t * C.t [@@deriving yojson]
end

module JQuad (A : JSONABLE) (B : JSONABLE) (C : JSONABLE) (D : JSONABLE) : JSONABLE with type t = A.t * B.t * C.t * D.t = struct
  type t = A.t * B.t * C.t * D.t [@@deriving yojson]
end

(* Slugs *)

(* for the slugs, we can stringify them just like strings, no matter what type
   they carry. *)
module type TYPEABLE = sig type t end
module SSlug (A : TYPEABLE) : STRINGABLE with type t = A.t Slug.t = struct
  type t = A.t Slug.t
  let to_string = Slug.to_string
  let of_string = Slug.check_string
end
module JSlug (A : TYPEABLE) : JSONABLE with type t = A.t Slug.t = struct
  type t = A.t Slug.t
  let yojson_of_t = Slug.yojson_of_t'
  let t_of_yojson = Slug.t_of_yojson'
end
