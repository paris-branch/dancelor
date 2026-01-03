include Option

let choose ~tie first second =
  match first, second with
  | None, None -> None
  | Some x, None | None, Some x -> Some x
  | Some x, Some y -> Some (tie x y)

let first = fun x _ -> x
let second = fun _ y -> y

let return = some

let of_string_nonempty = function
  | "" -> None
  | s -> Some s

let fold' ~none ~some = function
  | None -> none ()
  | Some x -> some x

let value' ~default = function
  | None -> default ()
  | Some x -> x
