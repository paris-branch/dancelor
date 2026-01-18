open Nes

type public = Public [@@deriving eq, show, yojson]

module Private : sig
  type meta_visibility = [
    | `Owners_only
    | `Everyone
    | `Select_viewers of User.t Id.t NEList.t
  ]

  type content_visibility = [
    | `Same_as_meta_visibility
    | meta_visibility
  ]

  val select_viewers : User.t Id.t NEList.t -> [`Select_viewers of User.t Id.t NEList.t]

  type t [@@deriving eq, show, yojson]

  val make :
    owners: User.t Id.t NEList.t ->
    ?meta_visibility: meta_visibility ->
    ?content_visibility: content_visibility ->
    unit ->
    t

  val owners : t -> User.t Id.t NEList.t
  val meta_visibility : t -> meta_visibility
  val content_visibility : t -> content_visibility
end
