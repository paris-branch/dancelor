type t = N of string [@@deriving eq, show]

let to_string (N s) = s

let of_string = function "" -> None | s -> Some (N s)

let of_string_exn n =
  match of_string n with
  | None -> invalid_arg "NesNonEmptyString.of_string_exn: empty string"
  | Some n -> n

type mystring = string [@@deriving yojson]

let to_yojson (N s) = mystring_to_yojson s

let of_yojson json =
  Result.bind (mystring_of_yojson json) @@ fun string ->
  Option.to_result ~none: "empty string" (of_string string)

let map f (N s) = of_string (f s)

let map_exn f n =
  match map f n with
  | None -> invalid_arg "NesNonEmptyString.map_exn: empty string"
  | Some n -> n

let of_char c = N (String.make 1 c)
