open Nes

type public = Public [@@deriving eq, show, yojson]

module Private : sig
  type t [@@deriving eq, show, yojson]

  val make : owners: User.t Id.t NEList.t -> t

  val owners : t -> User.t Id.t NEList.t
end
