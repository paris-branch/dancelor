open Nes

type public =
  Public
[@@deriving eq, show, yojson]

module Private = struct
  type visibility = [
    | `Owners_only
    | `Everyone
    | `Select_viewers of User.t Id.t NEList.t
  ]
  [@@deriving eq, show, variants, yojson]

  type t = {
    owners: User.t Id.t NEList.t;
    visibility: visibility; [@default `Owners_only]
  }
  [@@deriving eq, make, show, fields, yojson]
end
