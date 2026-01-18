open Nes

type public = Public [@@deriving eq, show, yojson]

module Private : sig
  type visibility = [
    | `Owners_only
    | `Everyone
    | `Select_viewers of User.t Id.t NEList.t
  ]

  val select_viewers : User.t Id.t NEList.t -> visibility

  type t [@@deriving eq, show, yojson]

  val make : owners: User.t Id.t NEList.t -> ?visibility: visibility -> unit -> t

  val owners : t -> User.t Id.t NEList.t
  val visibility : t -> visibility
end
