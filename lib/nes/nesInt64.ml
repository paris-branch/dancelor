include Int64

let nums =
  let nums = [| (* 0 -> 19  *)
    ""; " one"; " two"; " three"; " four";
    " five"; " six"; " seven"; " eight"; " nine";
    " ten"; " eleven"; " twelve"; " thirteen"; " fourteen";
    " fifteen"; " sixteen"; " seventeen"; " eighteen"; " nineteen"
  |] in
  fun n -> nums.(to_int n)

let tens =
  let tens = [| (* 0, 10 -> 90 *)
    ""; " ten"; " twenty"; " thirty"; " forty";
    " fifty"; " sixty"; " seventy"; " eighty"; " ninety"
  |] in
  fun n -> tens.(to_int n)

let to_english_string =
  (* first a version only for numbers between 0 and 1000 *)
  fun number ->
    assert (zero <= number && number < 1_000L);
    let so_far = ref "" in
    let number = ref number in
    if rem !number 100L < 20L then
      (
        so_far := nums (rem !number 100L);
        number := div !number 100L
      )
    else
      (
        so_far := nums (rem !number 10L);
        number := div !number 10L;
        so_far := tens (rem !number 10L) ^ !so_far;
        number := div !number 10L
      );
    if !number = 0L then !so_far
    else nums !number ^ " hundred" ^ !so_far

let to_english_string number =
  (* generic version for positive numbers *)
  (* FIXME: would not work on int32 systems; so maybe this function should be
     defined in the int64 module and used from here with a conversion *)
  assert (0L <= number);
  if number = 0L then
    "zero"
  else
    (
      let result =
        (let quintillions = rem (div number 1_000_000_000_000_000_000L) 1_000L in
         match quintillions with
         | 0L -> ""
         | 1L -> "one quintillion "
         | _ -> to_english_string quintillions ^ " quintillion ")
        ^
        (let quadrillions = rem (div number 1_000_000_000_000_000L) 1_000L in
         match quadrillions with
         | 0L -> ""
         | 1L -> "one quadrillion "
         | _ -> to_english_string quadrillions ^ " quadrillion ")
        ^
        (let trillions = rem (div number 1_000_000_000_000L) 1_000L in
         match trillions with
         | 0L -> ""
         | 1L -> "one trillion "
         | _ -> to_english_string trillions ^ " trillion ")
        ^
        (let billions = rem (div number 1_000_000_000L) 1_000L in
         match billions with
         | 0L -> ""
         | 1L -> "one billion "
         | _ -> to_english_string billions ^ " billion ")
        ^
        (let millions  = rem (div number 1_000_000L) 1_000L in
         match millions with
         | 0L -> ""
         | 1L -> "one million "
         | _ -> to_english_string millions ^ " million ")
        ^
        (let thousands = rem (div number 1_000L) 1_000L in
         match thousands with
         | 0L -> ""
         | 1L -> "one thousand "
         | _ -> to_english_string thousands ^ " thousand ")
        ^
        (let ones = rem number 1_000L in
         to_english_string ones)
      in
      NesString.remove_duplicates ~char:' ' result
    )

let to_english_string number =
  assert (number <> min_int);
  (if number >= 0L then "" else "minus ")
  ^ to_english_string (abs number)

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
let%test _ = to_english_string 123_456_789L =
             "one hundred twenty three million "
             ^ "four hundred fifty six thousand seven hundred eighty nine"
let%test _ = to_english_string 2_147_483_647L =
             "two billion one hundred forty seven million "
             ^ "four hundred eighty three thousand six hundred forty seven"
let%test _ = to_english_string 3_000_000_010L = "three billion ten"

let to_english_times_string = function
  | 1L -> "once"
  | 2L -> "twice"
  | n -> to_english_string n ^ " times"

(* FIXME: to_english_nth_string *)
