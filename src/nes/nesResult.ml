include Stdlib.Result

let of_string_nonempty ~empty = function
  | "" -> Error empty
  | s -> Ok s
