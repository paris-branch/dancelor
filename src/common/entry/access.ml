type public =
  Public
[@@deriving eq, show, yojson]

type private_ = {
  owner: User.t Id.t;
}
[@@deriving eq, make, show, fields, yojson]

let make_public_ignore ~owner: _ = Public
