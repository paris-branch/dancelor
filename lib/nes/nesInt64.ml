include Int64

let nums_to_english_string =
  let nums =
    [|
      (* 0 -> 19  *)
      [];
      ["one"];
      ["two"];
      ["three"];
      ["four"];
      ["five"];
      ["six"];
      ["seven"];
      ["eight"];
      ["nine"];
      ["ten"];
      ["eleven"];
      ["twelve"];
      ["thirteen"];
      ["fourteen"];
      ["fifteen"];
      ["sixteen"];
      ["seventeen"];
      ["eighteen"];
      ["nineteen"];
    |]
  in
  fun n -> nums.(to_int n)

let tens_to_english_string =
  let tens =
    [|
      (* 0, 10 -> 90 *)
      [];
      ["ten"];
      ["twenty"];
      ["thirty"];
      ["forty"];
      ["fifty"];
      ["sixty"];
      ["seventy"];
      ["eighty"];
      ["ninety"];
    |]
  in
  fun n -> tens.(to_int n)

let ones_to_english_string number =
  assert (0L <= number && number < 1_000L);
  let ones_and_tens =
    if rem number 100L < 20L then
      nums_to_english_string (rem number 100L)
    else
      tens_to_english_string (rem (div number 10L) 10L)
      @ nums_to_english_string (rem number 10L)
  in
  if number < 100L then
    ones_and_tens
  else
    nums_to_english_string (div number 100L)
    @ ["hundred"] @ ones_and_tens

let to_english_string_unsigned number =
  (* the following code works up to quintillions, that is for any number
     strictly below 1000 * 10^18, which is enough for 2^64 *)
  let rem = unsigned_rem in
  let div = unsigned_div in
  if number = 0L then
    ["zero"]
  else
    (
      (
        let quintillions = rem (div number 1_000_000_000_000_000_000L) 1_000L in
        match quintillions with
        | 0L -> []
        | 1L -> ["one"; "quintillion"]
        | _ -> ones_to_english_string quintillions @ ["quintillion"]
      )
      @ (
        let quadrillions = rem (div number 1_000_000_000_000_000L) 1_000L in
        match quadrillions with
        | 0L -> []
        | 1L -> ["one"; "quadrillion"]
        | _ -> ones_to_english_string quadrillions @ ["quadrillion"]
      )
      @ (
        let trillions = rem (div number 1_000_000_000_000L) 1_000L in
        match trillions with
        | 0L -> []
        | 1L -> ["one"; "trillion"]
        | _ -> ones_to_english_string trillions @ ["trillion"]
      )
      @ (
        let billions = rem (div number 1_000_000_000L) 1_000L in
        match billions with
        | 0L -> []
        | 1L -> ["one"; "billion"]
        | _ -> ones_to_english_string billions @ ["billion"]
      )
      @ (
        let millions = rem (div number 1_000_000L) 1_000L in
        match millions with
        | 0L -> []
        | 1L -> ["one"; "million"]
        | _ -> ones_to_english_string millions @ ["million"]
      )
      @ (
        let thousands = rem (div number 1_000L) 1_000L in
        match thousands with
        | 0L -> []
        | 1L -> ["one"; "thousand"]
        | _ -> ones_to_english_string thousands @ ["thousand"]
      )
      @ (
        let ones = rem number 1_000L in
        ones_to_english_string ones
      )
    )

let to_english_string_unsigned number =
  String.concat " " (to_english_string_unsigned number)

let to_english_string number =
  (* note: abs min_int = min_int but it is okay because min_int = max_int + 1 so
     the unsigned function will work just fine *)
  (if number >= 0L then "" else "minus ")
  ^ to_english_string_unsigned (abs number)

let%test _ = to_english_string 0L = "zero"
let%test _ = to_english_string 1L = "one"
let%test _ = to_english_string 16L = "sixteen"
let%test _ = to_english_string 100L = "one hundred"
let%test _ = to_english_string 118L = "one hundred eighteen"
let%test _ = to_english_string 200L = "two hundred"
let%test _ = to_english_string 219L = "two hundred nineteen"
let%test _ = to_english_string 800L = "eight hundred"
let%test _ = to_english_string 801L = "eight hundred one"
let%test _ = to_english_string 1_316L = "one thousand three hundred sixteen"
let%test _ = to_english_string 1_000_000L = "one million"
let%test _ = to_english_string 2_000_000L = "two million"
let%test _ = to_english_string 3_000_200L = "three million two hundred"
let%test _ = to_english_string 700_000L = "seven hundred thousand"
let%test _ = to_english_string 9_000_000L = "nine million"
let%test _ = to_english_string 9_001_000L = "nine million one thousand"
let%test _ =
  to_english_string 123_456_789L
  = "one hundred twenty three million"
  ^ " four hundred fifty six thousand"
  ^ " seven hundred eighty nine"
let%test _ =
  to_english_string 2_147_483_647L
  = "two billion"
  ^ " one hundred forty seven million"
  ^ " four hundred eighty three thousand"
  ^ " six hundred forty seven"
let%test _ = to_english_string 3_000_000_010L = "three billion ten"
let%test _ =
  to_english_string (-9_223_372_036_854_775_808L)
  = "minus nine quintillion"
  ^ " two hundred twenty three quadrillion"
  ^ " three hundred seventy two trillion"
  ^ " thirty six billion"
  ^ " eight hundred fifty four million"
  ^ " seven hundred seventy five thousand"
  ^ " eight hundred eight"

let to_english_string_times_unsigned = function
  | 1L -> "once"
  | 2L -> "twice"
  | n -> to_english_string_unsigned n ^ " times"

let to_english_string_times = function
  | 1L -> "once"
  | 2L -> "twice"
  | n -> to_english_string n ^ " times"

(* FIXME: to_english_string_nth *)
