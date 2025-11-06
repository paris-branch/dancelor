include Seq

let is_empty s =
  s () = Nil

let rec for_all p s =
  match s () with
  | Nil -> true
  | Cons (x, s) -> p x && for_all p s

let rec exists p s =
  match s () with
  | Nil -> false
  | Cons (x, s) -> p x || exists p s

let rec for_all2 p s1 s2 =
  match s1 (), s2 () with
  | Nil, Nil -> true
  | Cons (x1, s1), Cons (x2, s2) -> p x1 x2 && for_all2 p s1 s2
  | _ -> invalid_arg "NesSeq.for_all2"

let rec exists2 p s1 s2 =
  match s1 (), s2 () with
  | Nil, Nil -> false
  | Cons (x1, s1), Cons (x2, s2) -> p x1 x2 || exists2 p s1 s2
  | _ -> invalid_arg "NesSeq.exists2"

let rec compare cmp s1 s2 =
  match s1 (), s2 () with
  | Nil, Nil -> 0
  | Nil, _ -> -1
  | _, Nil -> 1
  | Cons (x1, s1), Cons (x2, s2) ->
    let c = cmp x1 x2 in
    if c <> 0 then c else compare cmp s1 s2

let return x = fun () -> Cons (x, fun () -> Nil)
