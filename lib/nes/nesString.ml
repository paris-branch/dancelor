open NesPervasives
include String

let pp = Format.pp_print_string

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

let starts_with ~needle haystack =
  try sub haystack 0 (length needle) = needle
  with Invalid_argument _ -> false

let%test _ = starts_with ~needle:"he" "hello"
let%test _ = not (starts_with ~needle:"hee" "hello")
let%test _ = not (starts_with ~needle:"hello" "he")

let ends_with ~needle haystack =
  let l = length needle in
  try sub haystack (length haystack - l) l = needle
  with Invalid_argument _ -> false

let%test _ = ends_with ~needle:"lo" "hello"
let%test _ = not (ends_with ~needle:"elo" "hello")
let%test _ = not (ends_with ~needle:"hello" "lo")

let remove_prefix ~needle haystack =
  let l = length needle in
  try Some (sub haystack l (length haystack - l))
  with Invalid_argument _ -> None

let%test _ = remove_prefix ~needle:"" "hello" = Some "hello"
let%test _ = remove_prefix ~needle:"he" "hello" = Some "llo"
let%test _ = remove_prefix ~needle:"hello" "hello" = Some ""
let%test _  =remove_prefix ~needle:"helloo" "hello" = None

let remove_suffix ~needle haystack =
  try Some (sub haystack 0 (length haystack - length needle))
  with Invalid_argument _ -> None

let%test _ = remove_suffix ~needle:"" "hello" = Some "hello"
let%test _ = remove_suffix ~needle:"lo" "hello" = Some "hel"
let%test _ = remove_suffix ~needle:"hello" "hello" = Some ""
let%test _  =remove_suffix ~needle:"hhello" "hello" = None

let inclusion_distance ~needle haystack =
  let ln = length needle in
  let lh = length haystack in
  let a = Array.make_matrix (ln+1) (lh+1) (-1) in
  let min3 a b c = min (min a b) c in
  let rec aux n h =
    if a.(n).(h) = -1 then
      (
        a.(n).(h) <-
          if n >= ln then
            0
          else if h >= lh then
            ln - n
          else
            (
              min3
                (1 + aux (n+1) h)
                ((if n = 0 then 0 else 1) + aux n (h+1))
                ((if needle.[n] = haystack.[h] then 0 else 1) + aux (n+1) (h+1))
            )
      );
    a.(n).(h)
  in
  aux 0 0

let%test _ = inclusion_distance ~needle:"chou" "achouffe" = 0
let%test _ = inclusion_distance ~needle:"chou" "acoul" = 1
let%test _ = inclusion_distance ~needle:"chou" "achhouffe" = 1
let%test _ = inclusion_distance ~needle:"chou" "achauffe" = 1

let inclusion_proximity ~needle haystack =
  let l = length needle in
  let n =
    1.
    -.
    if l = 0 then
      0.
    else
      let d = inclusion_distance ~needle haystack in
      (foi d) /. (foi l)
  in
  n *. n

let escape ?(esc='\\') ~chars s =
  let l = String.length s in
  let t = Bytes.create (2 * l) in
  let rec aux i j =
    if i >= l then
      (
        Bytes.sub_string t 0 j
      )
    else
      (
        if s.[i] = esc || String.contains chars s.[i] then
          (
            Bytes.set t j esc;
            Bytes.set t (j+1) s.[i];
            aux (i+1) (j+2)
          )
        else
          (
            Bytes.set t j s.[i];
            aux (i+1) (j+1)
          )
      )
  in
  aux 0 0

let%test _ = escape ~chars:"\"'" "Et j'lui ai dit \\: \"Yo, ç'va ?\"" = "Et j\\'lui ai dit \\\\: \\\"Yo, ç\\'va ?\\\""
