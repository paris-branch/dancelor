type 'a m = 'a option

let bind x f =
  match x with
  | Some x -> f x
  | None -> None

let (>>=) = bind

let compose f1 f2 x =
  f1 x >>= fun x1 -> f2 x1

let (>=>) = compose

let map f = function
  | None -> None
  | Some x -> Some (f x)

let unwrap = function
  | None -> failwith "unwrap"
  | Some x -> x

let wrap x = Some x

let value ~default = function
  | None -> default
  | Some x -> x

let wrap_fun f =
  fun x -> Some (f x)

let ifsome f = function
  | None -> ()
  | Some x -> f x

let assert_some = function
  | None -> failwith "assert_some"
  | Some v -> Some v
