type public =
  Public
[@@deriving eq, show, yojson]

module Private = struct
  type t = {
    owner: User.t Id.t;
  }
  [@@deriving eq, make, show, fields, yojson]
end
