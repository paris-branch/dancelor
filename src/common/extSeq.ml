include Seq

let rec sub n s =
  fun () ->
  if n <= 0 then
    Nil
  else
    match s () with
    | Nil -> Nil
    | Cons (e, s') -> Cons (e, sub (n-1) s')
