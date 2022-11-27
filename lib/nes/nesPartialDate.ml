open NesPervasives

let _key = "partial-date"

type t =
  | YearMonthDay of int * int * int
  | YearMonth of int * int
  | Year of int

let compare = compare

let check_month month = month >= 1 && month <= 12
let check_full_date ~year ~month ~day =
  try
    ignore (Dates_calc.Dates.make_date ~year ~month ~day);
    true
  with
    Dates_calc.Dates.InvalidDate -> false

let check = function
  | YearMonthDay (year, month, day) -> check_full_date ~year ~month ~day
  | YearMonth (_year, month) -> check_month month
  | Year _year -> true

let from_string s =
  try
    let date =
      let repr = String.split_on_char '-' s in
      assert (repr <> []);
      let year, repr = List.(hd repr, tl repr) in
      assert (String.length year = 4);
      let year = int_of_string year in
      match repr with
      | [] -> Year year
      | month :: repr ->
        assert (String.length month = 2);
        let month = int_of_string month in
        match repr with
        | [] -> YearMonth (year, month)
        | day :: repr ->
          assert (repr = []);
          assert (String.length day = 2);
          let day = int_of_string day in
          YearMonthDay (year, month, day)
    in
    assert (check date);
    date
  with
    Assert_failure _ | Failure _ -> failwith "NesPartialDate.from_string"

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
