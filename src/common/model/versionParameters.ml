
module Self = struct
  type t =
    { transposition : Transposition.t Parameter.t [@default Parameter.Undefined] ;
      clef : Music.clef Parameter.t               [@default Parameter.Undefined] }
  [@@deriving make, yojson]

  let _key = "version-parameters"
end
include Self

let make ?transposition () =
  let transposition = Option.map Parameter.defined (transposition : Transposition.t option) in
  make ?transposition ()

let none = `Assoc [] |> of_yojson |> Result.get_ok

let transposition p = p.transposition
let clef p = p.clef

let compose ~parent parameters =
  { transposition = Parameter.compose ~tie:Transposition.compose parent.transposition parameters.transposition ;
    clef = Parameter.compose ~tie:Parameter.most_recent parent.clef parameters.clef }
