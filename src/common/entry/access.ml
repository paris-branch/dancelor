type t = {
  owner: User.t Id.t;
}
[@@deriving make, show, fields, yojson]
