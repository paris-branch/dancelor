open NesPervasives.Syntax

type t = int * int * int

let none = (0, 0, 0)
let is_none = (=) none

let from_string s =
  match String.split_on_char '-' s |> List.map int_of_string_opt with
  | [Some year] ->
    (year, 0, 0)
  | [Some year; Some month]
    when month >= 1 && month <= 12
    ->
    (year, month, 0)
  | [Some year; Some month; Some day]
    when month >= 1 && month <= 12
         && day >= 1 && day <= 31
    ->
    (year, month, day)
  | _ ->
    failwith "NesDate.from_string"

let to_string (year, month, day) =
  if year = 0 then
    ""
  else if month = 0 then
    spf "%04d" year
  else if day = 0 then
    spf "%04d-%02d" year month
  else
    spf "%04d-%02d-%02d" year month day

let to_yojson date =
  `String (to_string date)

let of_yojson = function
  | `String s ->
    (try Ok (from_string s)
     with _ -> Error "NesDate.of_yojson: not a valid date")
  | _ -> Error "NesDate.of_yojson: not a JSON string"

let compare = compare
