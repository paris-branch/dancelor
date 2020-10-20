
module Self = struct
  type t =
    { every_set : SetParameters.t [@key "every-set"] [@default SetParameters.none] }
  [@@deriving yojson]

  let _key = "program-parameters"
end
include Self

let none =
  { every_set = SetParameters.none }
