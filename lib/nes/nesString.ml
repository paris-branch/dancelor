open NesPervasives open Syntax
include String

let pp = Format.pp_print_string

let remove_char ?(char_equal=Char.equal) c s =
  let b = Bytes.create (length s) in
  let j = ref 0 in
  iter
    (fun c' ->
      if not (char_equal c c') then
        (Bytes.set b !j c'; incr j))
    s;
  Bytes.sub_string b 0 !j

let%test _ = remove_char '%' "Cou%cou, %%je s%ui%%s N%i%ols%" = "Coucou, je suis Niols"

let split n s =
  (sub s 0 n, sub s n (length s - n))

let%test _ = split 2 "hello" = ("he", "llo")

let starts_with ?(equal=equal) ~needle haystack =
  try equal (sub haystack 0 (length needle)) needle
  with Invalid_argument _ -> false

let%test _ = starts_with ~needle:"he" "hello"
let%test _ = not (starts_with ~needle:"hee" "hello")
let%test _ = not (starts_with ~needle:"hello" "he")

let ends_with ?(equal=equal) ~needle haystack =
  let l = length needle in
  try equal (sub haystack (length haystack - l) l) needle
  with Invalid_argument _ -> false

let%test _ = ends_with ~needle:"lo" "hello"
let%test _ = not (ends_with ~needle:"elo" "hello")
let%test _ = not (ends_with ~needle:"hello" "lo")

let remove_prefix_exn ~needle haystack =
  let l = length needle in
  sub haystack l (length haystack - l)

let remove_prefix ~needle haystack =
  try Some (remove_prefix_exn ~needle haystack)
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

let inclusion_distance ?(char_equal=Char.equal) ~needle haystack =
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
                ((if char_equal needle.[n] haystack.[h] then 0 else 1) + aux (n+1) (h+1))
            )
      );
    a.(n).(h)
  in
  aux 0 0

let%test _ = inclusion_distance ~needle:"chou" "achouffe" = 0
let%test _ = inclusion_distance ~needle:"chou" "acoul" = 1
let%test _ = inclusion_distance ~needle:"chou" "achhouffe" = 1
let%test _ = inclusion_distance ~needle:"chou" "achauffe" = 1

let inclusion_proximity ?char_equal ~needle haystack =
  let l = length needle in
  1. -. (
    if l = 0 then 0.
    else
      let d = inclusion_distance ?char_equal ~needle haystack in
      (foi d) /. (foi l)
  )

let escape ?(esc='\\') ~chars s =
  let l = length s in
  let t = Bytes.create (2 * l) in
  let rec aux i j =
    if i >= l then
      (
        Bytes.sub_string t 0 j
      )
    else
      (
        if s.[i] = esc || contains chars s.[i] then
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

let exists p s = to_seq s |> NesSeq.exists p
let for_all p s = to_seq s |> NesSeq.for_all p

module Sensible = struct

  (* We would be tempted to define [equal] as [compare = 0]. But since compare
     is sensible to prefix, it needs [equal] to be computed. *)
  let equal s1 s2 =
    NesSeq.for_all2 NesChar.Sensible.equal (to_seq s1) (to_seq s2)

  (* FIXME: sensible comparison of string should also extract integers and
     compare them numerically. *)

  let extract_prefix s =
    let prefixes = [ "a "; "the "; "la "; "le "; "les "; "l'" ] in
    let has_prefix prefix = starts_with ~equal ~needle:prefix s in
    match List.find_opt has_prefix prefixes with
    | None -> ("", s)
    | Some prefix -> split (length prefix) s

  let%test _ = extract_prefix "A Case Apart" = ("A ", "Case Apart")
  let%test _ = extract_prefix "les tests c'est chiant" = ("les ", "tests c'est chiant")
  let%test _ = extract_prefix "L'abricot magique" = ("L'", "abricot magique")

  let rec extract_head_number n s =
    match s() with
    | NesSeq.Cons(c, s') when NesChar.is_digit c ->
      extract_head_number (10 * n + int_of_char c) s'
    | _ -> (n, s)

  let rec compare s1 s2 =
    match s1(), s2() with
    | Seq.Nil, Seq.Nil -> 0
    | Nil, _ -> -1
    | _, Nil -> 1
    | Cons(c1, s1'), Cons(c2, s2') ->
      if NesChar.is_digit c1 && NesChar.is_digit c2 then
        (
          let (n1, s1') = extract_head_number 0 s1 in
          let (n2, s2') = extract_head_number 0 s2 in
          compare_or (Int.compare n1 n2) @@ fun () -> compare s1' s2'
        )
      else
        compare_or (NesChar.Sensible.compare c1 c2) @@ fun () -> compare s1' s2'

  let compare s1 s2 =
    compare (to_seq s1) (to_seq s2)

  let compare s1 s2 =
    let (p1, s1) = extract_prefix s1 in
    let (p2, s2) = extract_prefix s2 in
    compare_or (compare s1 s2) @@ fun () -> compare p1 p2
end
