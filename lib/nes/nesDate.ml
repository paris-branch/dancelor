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

let _key = "full-date"

type t = [
  | `YearMonthDay of int * int * int
]

let check_full = check_gen false

let from_string str =
  match from_string_gen check_full "NesDate.Full.from_string" str with
  | `YearMonthDay _ as date -> date
  | _ -> assert false
