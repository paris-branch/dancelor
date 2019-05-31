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

let starts_with needle haystack =
  try
    String.sub haystack 0 (String.length needle) = needle
  with
    Invalid_argument _ -> false

let%test _ = starts_with "he" "hello"
let%test _ = not (starts_with "hee" "hello")
let%test _ = not (starts_with "hello" "he")

let inclusion_distance needle haystack =
  let ln = String.length needle in
  let lh = String.length haystack in
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

let%test _ = inclusion_distance "chou" "achouffe" = 0
let%test _ = inclusion_distance "chou" "acoul" = 1
let%test _ = inclusion_distance "chou" "achhouffe" = 1
let%test _ = inclusion_distance "chou" "achauffe" = 1

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