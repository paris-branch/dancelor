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
