open Nes

type public =
  Public
[@@deriving eq, show, yojson]

module Private = struct
  type meta_visibility = [
    | `Owners_only
    | `Everyone
    | `Select_viewers of User.t Id.t NEList.t
  ]
  [@@deriving eq, show, variants, yojson]

  type content_visibility = [
    | `Same_as_meta_visibility
    | meta_visibility
  ]
  [@@deriving eq, show, yojson]

  type t = {
    owners: User.t Id.t NEList.t;
    meta_visibility: meta_visibility; [@default `Owners_only]
    content_visibility: content_visibility; [@default `Same_as_meta_visibility]
  }
  [@@deriving eq, make, show, fields, yojson]
end
