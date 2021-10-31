(* Base *)

type base_predicate =
  | IsBase of Kind.base
[@@deriving yojson]

type base = base_predicate Formula.t
[@@deriving yojson]

let isBase kind = Formula.pred (IsBase kind)

let accepts_base filter kind =
  Formula.interpret filter @@ function

  | IsBase kind' ->
    Lwt.return (Formula.interpret_bool (kind = kind'))

(* Version *)

type version_predicate =
  | IsVersion of Kind.version
  | Base of base
[@@deriving yojson]

type version = version_predicate Formula.t
[@@deriving yojson]

let isVersion kind = Formula.pred (IsVersion kind)
let base bfilter = Formula.pred (Base bfilter)

let accepts_version filter kind =
  Formula.interpret filter @@ function

  | IsVersion kind' ->
    Lwt.return (Formula.interpret_bool (kind = kind'))

  | Base bfilter ->
    let (_bars, bkind) = kind in
    accepts_base bfilter bkind

(* Dance *)

type dance_predicate =
  | IsDance of Kind.dance
  | Simple
  | Version of version
[@@deriving yojson]

type dance = dance_predicate Formula.t
[@@deriving yojson]

let isDance kind = Formula.pred (IsDance kind)
let version vfilter = Formula.pred (Version vfilter)

let accepts_dance filter kind =
  Formula.interpret filter @@ function

  | IsDance kind' ->
    Lwt.return (Formula.interpret_bool (kind = kind'))

  | Simple ->
    (match kind with
     | _, [_] -> Lwt.return Formula.interpret_true
     | _ -> Lwt.return Formula.interpret_false)

  | Version vfilter ->
    (match kind with
     | _, [vkind] -> accepts_version vfilter vkind
     | _ -> Lwt.return Formula.interpret_false)
