
module Self = struct
  type t =
    | FromTo of string * string
    | To of string
  [@@deriving yojson]

  let _key = "transposition"
end
include Self
