
module Self = struct
  type t =
    { transposition : Transposition.t Parameter.t [@default Parameter.Undefined] }
  [@@deriving make, yojson]

  let _key = "version-parameters"
end
include Self

let make ?transposition () =
  let transposition = Option.map Parameter.defined (transposition : Transposition.t option) in
  make ?transposition ()

let none = `Assoc [] |> of_yojson |> Result.get_ok

let transposition p = p.transposition

let compose ~parent parameters =
  { transposition =
      Parameter.compose
        ~tie:Transposition.compose
        parent.transposition
        parameters.transposition }
