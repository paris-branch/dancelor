include Seq

let rec sub n s =
  fun () ->
  if n <= 0 then
    Nil
  else
    match s () with
    | Nil -> Nil
    | Cons (e, s') -> Cons (e, sub (n-1) s')

let is_empty s =
  s () = Nil

let hd_opt s =
  match s () with
  | Nil -> None
  | Cons (x, _) -> Some x

let rec exists p s =
  match s () with
  | Nil -> false
  | Cons (x, s') -> p x || exists p s'

let rec for_all p s =
  match s () with
  | Nil -> true
  | Cons (x, s') -> p x && for_all p s'
