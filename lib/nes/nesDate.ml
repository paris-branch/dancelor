open NesPervasives

type t = int * int * int

let from_string s =
  match String.split_on_char '-' s |> List.map int_of_string_opt with
  | [Some year; Some month; Some day]
    when month >= 1 && month <= 12
         && day >= 1 && day <= 31
    ->
    (year, month, day)
  | _ ->
    failwith "Dancelor_common.Date.from_string"

let to_string (year, month, day) =
  spf "%04d-%02d-%02d" year month day

let to_jsonm date =
  `String (to_string date)
