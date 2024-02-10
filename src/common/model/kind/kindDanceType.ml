type t =
  | Version of KindVersion.t
  | Add of t * t
  | Mul of int * t
[@@deriving eq, show {with_path = false}]

let version kv = Version kv
let add k1 k2 = Add (k1, k2)
let mul n k = Mul (n, k)

let gen =
  let open QCheck.Gen in
  sized @@ fix @@ fun self -> function
  | 0 -> version <$> KindVersion.gen
  | n -> oneof [
      (add <$> self (n/2) <*> self (n/2));
      (mul <$> nat <*> self (n-1));
    ]
