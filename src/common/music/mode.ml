type t =
  Major | Minor
[@@deriving eq, show {with_path = false}]

let to_string = function Major -> "" | Minor -> "m"

let to_lilypond_string = function
  | Major -> "\\major"
  | Minor -> "\\minor"
