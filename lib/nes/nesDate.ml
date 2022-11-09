open NesPervasives

type t = int * int * int
type full = t

let none = (0, 0, 0)
let is_none = (=) none

let check_day day = day >= 1 && day <= 31
let check_month month = month >= 1 && month <= 12
let check_year _year = true

let check_gen allow_none allow_partial = function
  | (0, 0, 0) -> allow_none
  | (year, 0, 0) -> allow_partial && check_year year
  | (year, month, 0) -> allow_partial && check_year year && check_month month
  | (year, month, day) -> check_year year && check_month month && check_day day

let check = check_gen true true
let check_full = check_gen false false

let from_string_gen check_ name s =
  let date =
    match String.split_on_char '-' s |> List.map int_of_string_opt with
    | [Some year] -> (year, 0, 0)
    | [Some year; Some month] -> (year, month, 0)
    | [Some year; Some month; Some day] -> (year, month, day)
    | _ -> failwith name
  in
  if not (check_ date) then failwith name;
  date

let from_string = from_string_gen check "NesDate.from_string"
let from_string_full = from_string_gen check_full "NesDate.from_string_full"

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

let month_to_pretty_string month =
  [| "January"; "February"; "March"; "April"; "May"; "June"; "July";
     "August"; "September"; "October"; "November"; "December" |].(month - 1)

let to_pretty_string ?(at=false) (year, month, day) =
  if year = 0 then
    ""
  else if month = 0 then
    spf "%s%d" (if at then "in " else "") year
  else if day = 0 then
    spf "%s%s %d" (if at then "in " else "") (month_to_pretty_string month) year
  else
    spf "%s%d %s %d" (if at then "on " else "") day (month_to_pretty_string month) year
