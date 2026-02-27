include Uri

let to_yojson u = `String (to_string u)

let of_yojson = function
  | `String s -> Ok (of_string s)
  | _ -> Error "not a string"
