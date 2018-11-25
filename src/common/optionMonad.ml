type 'a m = 'a option

let bind x f =
  match x with
  | Some x -> f x
  | None -> None

let (>>=) = bind
