open Nes

(** Type and serialisers to/from Uri-safe string. *)
module type STRINGABLE = sig
  type t
  val to_string : t -> string
  val of_string : string -> t option
end

(** Poorly named, contains a type and serialisers to/from Biniou and Yojson. *)
module type JSONABLE = sig
  type t [@@deriving biniou, yojson]
end

(* Base *)

module SString : STRINGABLE with type t = string = struct
  type t = string
  let to_string = Fun.id
  let of_string = some
end

module SUnit : STRINGABLE with type t = unit = struct
  type t = unit
  let to_string () = ""
  let of_string _ = Some ()
end

module SStatusCode : STRINGABLE with type t = Cohttp.Code.status_code = struct
  type t = Cohttp.Code.status_code
  let to_string = string_of_int % Cohttp.Code.code_of_status
  let of_string = Option.map Cohttp.Code.status_of_code % int_of_string_opt
end

module JString : JSONABLE with type t = string = struct
  type t = string [@@deriving biniou, yojson]
end

module JUri : JSONABLE with type t = Uri.t = struct
  type t = Uri.t
  let to_biniou = JString.to_biniou % Uri.to_string
  let of_biniou_exn = Uri.of_string % JString.of_biniou_exn
  let of_biniou = Result.map Uri.of_string % JString.of_biniou
  let to_yojson = JString.to_yojson % Uri.to_string
  let of_yojson = Result.map Uri.of_string % JString.of_yojson
end

module JUnit : JSONABLE with type t = unit = struct
  type t = unit [@@deriving biniou, yojson]
end

module JVoid : JSONABLE with type t = Void.t = struct
  type t = Void.t [@@deriving biniou, yojson]
end

module JBool : JSONABLE with type t = bool = struct
  type t = bool [@@deriving biniou, yojson]
end

module JInt : JSONABLE with type t = int = struct
  type t = int [@@deriving biniou, yojson]
end

module JFloat : JSONABLE with type t = float = struct
  type t = float [@@deriving biniou, yojson]
end

(* Higher order *)

module JOption (A : JSONABLE) : JSONABLE with type t = A.t option = struct
  type t = A.t option [@@deriving biniou, yojson]
end

module JList (A : JSONABLE) : JSONABLE with type t = A.t list = struct
  type t = A.t list [@@deriving biniou, yojson]
end

module JPair (A : JSONABLE) (B : JSONABLE) : JSONABLE with type t = A.t * B.t = struct
  type t = A.t * B.t [@@deriving biniou, yojson]
end

module JTriple (A : JSONABLE) (B : JSONABLE) (C : JSONABLE) : JSONABLE with type t = A.t * B.t * C.t = struct
  type t = A.t * B.t * C.t [@@deriving biniou, yojson]
end

module JQuad (A : JSONABLE) (B : JSONABLE) (C : JSONABLE) (D : JSONABLE) : JSONABLE with type t = A.t * B.t * C.t * D.t = struct
  type t = A.t * B.t * C.t * D.t [@@deriving biniou, yojson]
end
