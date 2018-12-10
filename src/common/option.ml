type 'a m = 'a option

let bind x f =
  match x with
  | Some x -> f x
  | None -> None

let (>>=) = bind

let map f = function
  | None -> None
  | Some x -> Some (f x)

let unwrap = function
  | None -> failwith "unwrap"
  | Some x -> x

let value ~default = function
  | None -> default
  | Some x -> x
