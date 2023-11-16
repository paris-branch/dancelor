include Option

let compose f1 f2 x =
  bind (f1 x) f2

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

let concat conc o1 o2 =
  match (o1, o2) with
  | (None, None) -> None
  | (Some v1, None) -> Some v1
  | (None, Some v2) -> Some v2
  | (Some v1, Some v2) -> Some (conc v1 v2)

let concat_l conc = List.fold_left (concat conc) None
