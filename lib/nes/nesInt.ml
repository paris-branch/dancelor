include Int

let to_english_string =
  (* first a version only for numbers between 0 and 1000 *)
  let nums = [| (* 0 -> 19  *)
    ""; " one"; " two"; " three"; " four";
    " five"; " six"; " seven"; " eight"; " nine";
    " ten"; " eleven"; " twelve"; " thirteen"; " fourteen";
    " fifteen"; " sixteen"; " seventeen"; " eighteen"; " nineteen"
  |] in
  let tens = [| (* 0, 10 -> 90 *)
    ""; " ten"; " twenty"; " thirty"; " forty";
    " fifty"; " sixty"; " seventy"; " eighty"; " ninety"
  |] in
  fun number ->
    assert (0 <= number && number < 1_000);
    let so_far = ref "" in
    let number = ref number in
    if !number mod 100 < 20 then
      (
        so_far := nums.(!number mod 100);
        number := !number / 100
      )
    else
      (
        so_far := nums.(!number mod 10);
        number := !number / 10;
        so_far := tens.(!number mod 10) ^ !so_far;
        number := !number / 10
      );
    if !number = 0 then !so_far
    else nums.(!number) ^ " hundred" ^ !so_far

let to_english_string number =
  (* generic version for positive numbers *)
  (* FIXME: would not work on int32 systems; so maybe this function should be
     defined in the int64 module and used from here with a conversion *)
  assert (0 <= number);
  if number = 0 then
    "zero"
  else
    (
      let result =
        (let quintillions = (number / 1_000_000_000_000_000_000) mod 1_000 in
         match quintillions with
         | 0 -> ""
         | 1 -> "one quintillion "
         | _ -> to_english_string quintillions ^ " quintillion ")
        ^
        (let quadrillions = (number / 1_000_000_000_000_000) mod 1_000 in
         match quadrillions with
         | 0 -> ""
         | 1 -> "one quadrillion "
         | _ -> to_english_string quadrillions ^ " quadrillion ")
        ^
        (let trillions = (number / 1_000_000_000_000) mod 1_000 in
         match trillions with
         | 0 -> ""
         | 1 -> "one trillion "
         | _ -> to_english_string trillions ^ " trillion ")
        ^
        (let billions = (number / 1_000_000_000) mod 1_000 in
         match billions with
         | 0 -> ""
         | 1 -> "one billion "
         | _ -> to_english_string billions ^ " billion ")
        ^
        (let millions  = (number / 1_000_000) mod 1_000 in
         match millions with
         | 0 -> ""
         | 1 -> "one million "
         | _ -> to_english_string millions ^ " million ")
        ^
        (let thousands = (number / 1_000) mod 1_000 in
         match thousands with
         | 0 -> ""
         | 1 -> "one thousand "
         | _ -> to_english_string thousands ^ " thousand ")
        ^
        (let ones = number mod 1_000 in
         to_english_string ones)
      in
      NesString.remove_duplicates ~char:' ' result
    )

let to_english_string number =
  assert (number <> min_int);
  (if number >= 0 then "" else "minus ")
  ^ to_english_string (abs number)

let%test _ = to_english_string 0 = "zero"
let%test _ = to_english_string 1 = "one"
let%test _ = to_english_string 16 = "sixteen"
let%test _ = to_english_string 100 = "one hundred"
let%test _ = to_english_string 118 = "one hundred eighteen"
let%test _ = to_english_string 200 = "two hundred"
let%test _ = to_english_string 219 = "two hundred nineteen"
let%test _ = to_english_string 800 = "eight hundred"
let%test _ = to_english_string 801 = "eight hundred one"
let%test _ = to_english_string 1_316 = "one thousand three hundred sixteen"
let%test _ = to_english_string 1_000_000 = "one million"
let%test _ = to_english_string 2_000_000 = "two million"
let%test _ = to_english_string 3_000_200 = "three million two hundred"
let%test _ = to_english_string 700_000 = "seven hundred thousand"
let%test _ = to_english_string 9_000_000 = "nine million"
let%test _ = to_english_string 9_001_000 = "nine million one thousand"
let%test _ = to_english_string 123_456_789 =
             "one hundred twenty three million "
             ^ "four hundred fifty six thousand seven hundred eighty nine"
let%test _ = to_english_string 2_147_483_647 =
             "two billion one hundred forty seven million "
             ^ "four hundred eighty three thousand six hundred forty seven"
let%test _ = to_english_string 3_000_000_010 = "three billion ten"

let to_english_times_string = function
  | 1 -> "once"
  | 2 -> "twice"
  | n -> to_english_string n ^ " times"

(* FIXME: to_english_nth_string *)
