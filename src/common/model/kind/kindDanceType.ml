type t =
  | Version of KindVersion.t
  | Add of t * t
  | Mul of int * t
[@@deriving show {with_path = false}]

let rec normalise = function
  | Version k -> Version k
  | Mul (n, k) -> Mul (n, normalise k)
  | Add (Add (k1, k2), k3) -> normalise @@ Add (k1, Add (k2, k3))
  | Add (k1, k2) -> Add (normalise k1, normalise k2)

let rec equal k1 k2 =
  match (k1, k2) with
  | Version k1, Version k2 -> KindVersion.equal k1 k2
  | Add (k11, k12), Add (k21, k22) -> equal k11 k21 && equal k12 k22
  | Mul (n1, k1), Mul (n2, k2) -> n1 = n2 && equal k1 k2
  | _ -> false
let equal k1 k2 = equal (normalise k1) (normalise k2)

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

let rec shrink = let open QCheck.Iter in function
    | Version kv -> map version (KindVersion.shrink kv)
    | Add (k1, k2) -> return k1 <+> return k2
                      <+> map (Fun.flip add k2) (shrink k1)
                      <+> map (add k1) (shrink k2);
    | Mul (n, k) -> return k <+> map (mul n) (shrink k)
