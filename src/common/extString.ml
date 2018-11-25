include String

let remove_char c s =
  let b = Bytes.create (length s) in
  let j = ref 0 in
  iter
    (fun c' ->
      if c <> c' then
        (Bytes.set b !j c'; incr j))
    s;
  Bytes.sub_string b 0 !j

let%test _ = remove_char '%' "Cou%cou, %%je s%ui%%s N%i%ols%" = "Coucou, je suis Niols"

let split n s =
  (String.sub s 0 n, String.sub s n (String.length s - n))

let%test _ = split 2 "hello" = ("he", "llo")

let starts_with needle haystack =
  try
    String.sub haystack 0 (String.length needle) = needle
  with
    Invalid_argument _ -> false

let%test _ = starts_with "he" "hello"
let%test _ = not (starts_with "hee" "hello")
let%test _ = not (starts_with "hello" "he")
