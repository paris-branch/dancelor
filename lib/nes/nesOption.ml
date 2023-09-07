include Option

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

let unwrap_or ~default = function
  | None -> default
  | Some x -> x

let unwrap_map_or ~default f = function
  | None -> default
  | Some x -> f x

let wrap x = Some x

let wrap_fun f = fun x ->
  Some (f x)

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

let choose ~tie first second =
  match first, second with
  | None, None -> None
  | Some x, None | None, Some x -> Some x
  | Some x, Some y -> Some (tie x y)

let fail = fun _ _ -> failwith "NesOption.choose ~tie:fail"
let second = fun _ y -> y

let compare_lwt cmp o0 o1 =
  match o0, o1 with
  | Some v0, Some v1 -> cmp v0 v1
  | None, None -> Lwt.return 0
  | None, Some _ -> Lwt.return (-1)
  | Some _, None -> Lwt.return 1
