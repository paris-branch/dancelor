type t =
  | Version of Kind_version.t
  | Add of t * t
  | Mul of int * t
[@@deriving eq, ord, show {with_path = false}]

let rec normalise = function
  | Version k -> Version k
  | Mul (n, k) -> Mul (n, normalise k)
  | Add (Add (k1, k2), k3) -> normalise @@ Add (k1, Add (k2, k3))
  | Add (k1, k2) -> Add (normalise k1, normalise k2)

let equal k1 k2 = equal (normalise k1) (normalise k2)
let compare k1 k2 = compare (normalise k1) (normalise k2)

let version kv = Version kv
let add k1 k2 = Add (k1, k2)
let mul n k = Mul (n, k)
