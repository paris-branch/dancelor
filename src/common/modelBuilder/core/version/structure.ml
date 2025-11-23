open Nes

type t = Part_name.t NEList.t
[@@deriving eq, show {with_path = false}]

let to_string = NEString.of_string_exn % String.of_seq % Seq.map Part_name.to_char % List.to_seq % NEList.to_list
let of_string = Option.join % Option.map NEList.of_list % Monadise.Option.monadise_1_1 List.map Part_name.of_char % List.of_seq % String.to_seq % NEString.to_string

let to_yojson s = `String (NEString.to_string @@ to_string s)
let of_yojson = function
  | `String s -> Option.to_result ~none: "not a valid structure" (Option.bind (NEString.of_string s) of_string)
  | _ -> Error "not a string"
