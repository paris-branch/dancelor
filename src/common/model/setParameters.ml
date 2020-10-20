
module Self = struct
  type t =
    { every_version : VersionParameters.t [@key "every-version"] [@default VersionParameters.none ]}
  [@@deriving yojson]

  let _key = "set-parameters"
end
include Self

let none =
  { every_version = VersionParameters.none }
