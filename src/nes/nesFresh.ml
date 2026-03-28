module type S = sig
  type t [@@deriving eq, ord, show, yojson]
end

module Make (Base : S) : sig
    type t [@@deriving eq, ord, show, yojson]
    val inject : Base.t -> t
    val project : t -> Base.t
  end
= struct
  type t = Base.t [@@deriving eq, ord, show, yojson]
  let inject x = x
  let project x = x
end
