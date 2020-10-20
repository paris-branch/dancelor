
module Self = struct
  type t =
    | Relative of string * string
    | Absolute of string
  [@@deriving yojson]

  let _key = "transposition"
end
include Self

let relative a b = Relative (a, b)
let absolute a = Absolute a

let identity = relative "c" "c"

let compose t1 t2 =
  match t1, t2 with
  | _, Absolute a2 -> Absolute a2
  | _ -> assert false (* FIXME *)
