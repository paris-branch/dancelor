
module Self = struct
  type t =
    { transposition : Transposition.t Parameter.t [@default Parameter.Undefined] ;
      clef : Music.clef Parameter.t               [@default Parameter.Undefined] ;
      first_bar : int Parameter.t                 [@default Parameter.Undefined] [@key "first-bar"] ;
      display_name : string Parameter.t           [@default Parameter.Undefined] [@key "display-name"] ;
      display_author : string Parameter.t         [@default Parameter.Undefined] [@key "display-author"] }
  [@@deriving make, yojson]

  let _key = "version-parameters"
end
include Self

let make ?transposition ?clef ?first_bar () =
  let transposition = Option.map Parameter.defined transposition in
  let clef = Option.map Parameter.defined clef in
  make ?transposition ?clef ?first_bar ()

let none = `Assoc [] |> of_yojson |> Result.get_ok

let transposition p = p.transposition
let clef p = p.clef
let first_bar p = p.first_bar
let display_name p = p.display_name
let display_author p = p.display_author

let compose ~parent parameters =
  {
    transposition = Parameter.compose ~tie:Transposition.compose parent.transposition parameters.transposition ;
    clef = Parameter.compose ~tie:Parameter.most_recent parent.clef parameters.clef ;
    first_bar = Parameter.compose ~tie:Parameter.most_recent parent.first_bar parameters.first_bar ;
    display_name = Parameter.compose ~tie:Parameter.most_recent parent.display_name parameters.display_name ;
    display_author = Parameter.compose ~tie:Parameter.most_recent parent.display_author parameters.display_author ;
  }
