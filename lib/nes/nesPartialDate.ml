open NesPervasives

let _key = "partial-date"

type t = [
  | `YearMonthDay of int * int * int
  | `YearMonth of int * int
  | `Year of int
]

let check_day day = day >= 1 && day <= 31
let check_month month = month >= 1 && month <= 12
let check_year _year = true

let check_gen allow_partial = function
  | `YearMonthDay (year, month, day) -> check_year year && check_month month && check_day day
  | `YearMonth (year, month) -> allow_partial && check_year year && check_month month
  | `Year year -> allow_partial && check_year year

let from_string_gen check_ name s =
  let date =
    match String.split_on_char '-' s |> List.map int_of_string_opt with
    | [Some year] -> `Year year
    | [Some year; Some month] -> `YearMonth (year, month)
    | [Some year; Some month; Some day] -> `YearMonthDay (year, month, day)
    | _ -> failwith name
  in
  if not (check_ date) then failwith name;
  date

let check : t -> bool = check_gen true

let from_string : string -> t = from_string_gen check "NesDate.Partial.from_string"

let to_string : t -> string = function
  | `Year year -> spf "%04d" year
  | `YearMonth (year, month) -> spf "%04d-%02d" year month
  | `YearMonthDay (year, month, day) -> spf "%04d-%02d-%02d" year month day


let to_yojson date =
  `String (to_string date)

let of_yojson = function
  | `String s ->
    (try Ok (from_string s)
     with _ -> Error "NesDate.Partialof_yojson: not a valid date")
  | _ -> Error "NesDate.Partialof_yojson: not a JSON string"

let compare = compare

let month_to_pretty_string month =
  [| "January"; "February"; "March"; "April"; "May"; "June"; "July";
     "August"; "September"; "October"; "November"; "December" |].(month - 1)

let to_pretty_string ?(at=false) = function
  | `Year year -> spf "%s%d" (if at then "in " else "") year
  | `YearMonth (year, month) -> spf "%s%s %d" (if at then "in " else "") (month_to_pretty_string month) year
  | `YearMonthDay (year, month, day) -> spf "%s%d %s %d" (if at then "on " else "") day (month_to_pretty_string month) year
