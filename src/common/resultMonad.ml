type ('a, 'b) m = ('a, 'b) result

let bind x f =
  match x with
  | Ok x -> f x
  | Error e -> Error e

let (>>=) = bind
