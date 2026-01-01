open Nes

type public =
  Public
[@@deriving eq, show, yojson]

module Private = struct
  type t = {
    owners: User.t Id.t NEList.t;
  }
  [@@deriving eq, make, show, fields, yojson]
end
