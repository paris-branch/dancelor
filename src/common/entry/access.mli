type public = Public [@@deriving eq, show, yojson]

type private_ [@@deriving eq, show, yojson]

val make_private_ : owner: User.t Id.t -> private_

val make_public_ignore : owner: User.t Id.t -> public
(** Ignores its arguments and returns {!Public}. This function has the same
    signature as {!make_private} and can be used in the same places. *)

val owner : private_ -> User.t Id.t
