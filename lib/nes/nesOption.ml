type 'a t = 'a option

let bind x f =
  match x with
  | Some x -> f x
  | None -> None

let compose f1 f2 x =
  bind (f1 x) f2

let map f = function
  | None -> None
  | Some x -> Some (f x)

let unwrap = function
  | Some x -> x
  | None -> failwith "NesOption.unwrap"

let unwrap_or or_ = function
  | None -> or_
  | Some x -> x

let unwrap_map_or or_ f = function
  | None -> or_
  | Some x -> f x

let wrap x = Some x

let wrap_fun f =
  fun x -> Some (f x)

let ifsome f = function
  | None -> ()
  | Some x -> f x

let ifsome_lwt f = function
  | None -> Lwt.return ()
  | Some x -> f x

let assert_some = function
  | None -> failwith "assert_some"
  | Some v -> Some v

let assert_ b = if b then Some () else None

module Syntax = struct
  let (>>=?) = bind
  let (>=>?) = compose
end
