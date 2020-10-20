
module Self = struct
  type t =
    { transposition : Transposition.t Parameter.t [@default Parameter.Inherit] }
  [@@deriving yojson]

  let _key = "version-parameters"
end
include Self

let none =
  { transposition = Parameter.Inherit }
