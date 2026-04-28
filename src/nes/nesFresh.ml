module type S = sig
  type t [@@deriving eq, ord, show, yojson]
end

module type T = sig
  type base
  type t [@@deriving eq, ord, show, yojson]
  val inject : base -> t
  val project : t -> base
end

module Make (Base : S) : T with type base = Base.t = struct
  type base = Base.t
  type t = Base.t [@@deriving eq, ord, show, yojson]
  let inject x = x
  let project x = x
end
