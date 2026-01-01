type public = Public [@@deriving eq, show, yojson]

module Private : sig
  type t [@@deriving eq, show, yojson]

  val make : owner: User.t Id.t -> t

  val owner : t -> User.t Id.t
end
