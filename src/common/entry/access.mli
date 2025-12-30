type t [@@deriving show, yojson]

val make : owner: User.t Id.t -> t

val owner : t -> User.t Id.t
