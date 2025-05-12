open NesPervasives

type t =
  | YearMonthDay of int * int * int
  | YearMonth of int * int
  | Year of int
[@@deriving eq, show {with_path = false}]

let compare = compare

let check_month month = month >= 1 && month <= 12
let check_full_date ~year ~month ~day =
  try
    ignore (Dates_calc.Dates.make_date ~year ~month ~day);
    true
  with
    | Dates_calc.Dates.InvalidDate -> false

let check = function
  | YearMonthDay (year, month, day) -> check_full_date ~year ~month ~day
  | YearMonth (_year, month) -> check_month month
  | Year _year -> true

let from_string s =
  let date =
    match String.split_on_char '-' s |> List.map int_of_string_opt with
    | [Some year] -> Some (Year year)
    | [Some year; Some month] -> Some (YearMonth (year, month))
    | [Some year; Some month; Some day] -> Some (YearMonthDay (year, month, day))
    | _ -> None
  in
  match date with
  | Some date when check date -> Some date
  | _ -> None

let to_string = function
  | Year year -> spf "%04d" year
  | YearMonth (year, month) -> spf "%04d-%02d" year month
  | YearMonthDay (year, month, day) -> spf "%04d-%02d-%02d" year month day

let yojson_of_t date =
  `String (to_string date)

let t_of_yojson = function
  | `String s ->
    NesOption.value'
      (from_string s)
      ~default: (fun () -> NesJson.of_yojson_error "not a valid date" (`String s))
  | j -> NesJson.of_yojson_error "not a JSON string" j

let month_to_pretty_string month =
  [|
    "January";
    "February";
    "March";
    "April";
    "May";
    "June";
    "July";
    "August";
    "September";
    "October";
    "November";
    "December"
  |].(month - 1)

let to_pretty_string ?(at = false) = function
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
