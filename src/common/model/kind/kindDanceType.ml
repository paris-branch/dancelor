type t =
  | Version of KindVersion.t
  | Add of t * t
  | Mul of int * t
[@@deriving show {with_path = false}]
