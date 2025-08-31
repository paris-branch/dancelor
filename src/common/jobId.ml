open Nes

type t = string

let random_alphanumeral () =
  let n = Random.int 36 in
  if n < 10 then
    Char.chr (Char.code '0' + n)
  else if n < 36 then
    Char.chr (Char.code 'a' + n - 10)
  else
    assert false

let create () =
  String.init 20 (fun _ -> random_alphanumeral ())

let to_string = id

let to_yojson x = `String x

let is_alphanumeral c =
  ('0' <= c && c <= '9') || ('a' <= c && c <= 'z')

let of_string s =
  if String.for_all is_alphanumeral s then
    Some (id s)
  else
    None

let of_yojson = function
  | `String s -> Option.to_result ~none: "JobId.of_yojson" (of_string s)
  | _ -> Error "JobId.of_yojson"
