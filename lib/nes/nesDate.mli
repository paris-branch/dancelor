module Partial : sig
  val _key : string

  type t [@@deriving yojson]

  val from_string : string -> t
  val to_string : t -> string
  val to_pretty_string : ?at:bool -> t -> string

  val compare : t -> t -> int
end

module Full : sig
  val _key : string

  type t

  val from_string : string -> t
end
