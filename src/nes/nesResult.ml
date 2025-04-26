include Stdlib.Result

let of_string_nonempty ~empty = function
  | "" -> Error empty
  | s -> Ok s

let map_both ~ok ~error = function
  | Ok x -> Ok (ok x)
  | Error y -> Error (error y)
