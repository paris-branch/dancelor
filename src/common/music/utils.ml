let to_yojson__of__to_string to_string value =
  `String (to_string value)

let of_yojson__of__of_string of_string message = function
  | `String s ->
    (
      try
        Ok (of_string s)
      with
        | _ -> Error message
    )
  | _ -> Error message
