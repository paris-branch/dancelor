(** {1 Privacy}

    Levels of privacy are attached to entries in the database and influence who
    gets to see them. *)

type t =
  Private | Public
[@@deriving show, yojson]

let default = Private
