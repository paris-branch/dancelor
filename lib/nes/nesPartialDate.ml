open NesPervasives

let _key = "partial-date"

type t =
  | YearMonthDay of int * int * int
  | YearMonth of int * int
  | Year of int

let compare = compare

let check_day day = day >= 1 && day <= 31
let check_month month = month >= 1 && month <= 12
let check_year _year = true

let check = function
  | YearMonthDay (year, month, day) -> check_year year && check_month month && check_day day
  | YearMonth (year, month) -> check_year year && check_month month
  | Year year -> check_year year

let from_string s =
  let date =
    match String.split_on_char '-' s |> List.map int_of_string_opt with
    | [Some year] -> Year year
    | [Some year; Some month] -> YearMonth (year, month)
    | [Some year; Some month; Some day] -> YearMonthDay (year, month, day)
    | _ -> failwith "NesDate.Partial.from_string"
  in
  if not (check date) then failwith "NesDate.Partial.from_string";
  date

let to_string = function
  | Year year -> spf "%04d" year
  | YearMonth (year, month) -> spf "%04d-%02d" year month
  | YearMonthDay (year, month, day) -> spf "%04d-%02d-%02d" year month day

let to_yojson date =
  `String (to_string date)

let of_yojson = function
  | `String s ->
    (try Ok (from_string s)
     with _ -> Error "NesDate.Partialof_yojson: not a valid date")
  | _ -> Error "NesDate.Partialof_yojson: not a JSON string"

let month_to_pretty_string month =
  [| "January"; "February"; "March"; "April"; "May"; "June"; "July";
     "August"; "September"; "October"; "November"; "December" |].(month - 1)

let to_pretty_string ?(at=false) = function
  | Year year -> spf "%s%d" (if at then "in " else "") year
  | YearMonth (year, month) -> spf "%s%s %d" (if at then "in " else "") (month_to_pretty_string month) year
  | YearMonthDay (year, month, day) -> spf "%s%d %s %d" (if at then "on " else "") day (month_to_pretty_string month) year

(** {2 Helpers to Implement {!NesDate}}

    {!NesDate} can be seen as an instance of this one. It just needs this two
    functions to get it done. *)

let internal__from_full (year, month, day) =
  YearMonthDay (year, month, day)

let internal__to_full = function
  | YearMonthDay (year, month, day) -> Some (year, month, day)
  | _ -> None
