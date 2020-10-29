
module Self = struct
  type t =
    | Relative of Music.pitch * Music.pitch
    | Absolute of Music.pitch
  [@@deriving yojson]

  let _key = "transposition"
end
include Self

let relative a b = Relative (a, b)
let absolute a = Absolute a

let identity = relative Music.pitch_c Music.pitch_c

let compose t1 t2 =
  match t1, t2 with
  | _, Absolute a2 -> Absolute a2
  | _ -> assert false (* FIXME *)
