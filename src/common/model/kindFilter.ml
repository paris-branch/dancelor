open Nes

(* Base *)

module Base = struct
  type predicate =
    | Is of Kind.base
  [@@deriving yojson]

  type t = predicate Formula.t
  [@@deriving yojson]

  let is kind = Formula.pred (Is kind)

  let accepts filter kind =
    Formula.interpret filter @@ function

    | Is kind' ->
      Lwt.return (Formula.interpret_bool (kind = kind'))

  let raw string =
    match Kind.base_of_string_opt string with
    | Some kind -> Ok (is kind)
    | None -> error_fmt "could not interpret \"%s\" as a base kind" string

  let nullary_text_predicates = []

  let unary_text_predicates = []

  let from_text_formula =
    TextFormula.make_to_formula raw
      nullary_text_predicates
      unary_text_predicates
end

(* Version *)

module Version = struct
  type predicate =
    | Is of Kind.version
    | BarsEq of int | BarsGt of int | BarsLt of int
    | Base of Base.t
  [@@deriving yojson]

  type t = predicate Formula.t
  [@@deriving yojson]

  let is kind = Formula.pred (Is kind)
  let barsEq int = Formula.pred (BarsEq int)
  let barsNe int = Formula.not_ (barsEq int)
  let barsGt int = Formula.pred (BarsGt int)
  let barsGe int = Formula.or_l [ barsEq int; barsGt int ]
  let barsLt int = Formula.pred (BarsLt int)
  let barsLe int = Formula.or_l [ barsEq int; barsLt int ]

  let base bfilter = Formula.pred (Base bfilter)

  let accepts filter kind =
    Formula.interpret filter @@ function

    | Is kind' ->
      Lwt.return (Formula.interpret_bool (kind = kind'))

    | BarsEq bars' ->
      let (bars, _) = kind in
      Lwt.return (Formula.interpret_bool (bars = bars'))

    | BarsGt bars' ->
      let (bars, _) = kind in
      Lwt.return (Formula.interpret_bool (bars > bars'))

    | BarsLt bars' ->
      let (bars, _) = kind in
      Lwt.return (Formula.interpret_bool (bars < bars'))

    | Base bfilter ->
      let (_bars, bkind) = kind in
      Base.accepts bfilter bkind

  let raw string =
    match Kind.base_of_string_opt string with
    | Some bkind -> Ok (base (Base.is bkind))
    | None ->
      match Kind.version_of_string_opt string with
      | Some vkind -> Ok (is vkind)
      | None -> error_fmt "could not interpret \"%s\" as a kind for versions" string

  let nullary_text_predicates = []

  let unary_text_predicates = []

  let from_text_formula =
    TextFormula.make_to_formula raw
      nullary_text_predicates
      unary_text_predicates
end

(* Dance *)

module Dance = struct
  type predicate =
    | Is of Kind.dance
    | Simple
    | Version of Version.t
  [@@deriving yojson]

  type t = predicate Formula.t
  [@@deriving yojson]

  let is kind = Formula.pred (Is kind)
  let version vfilter = Formula.pred (Version vfilter)
  let base bfilter = version (Version.base bfilter)

  let accepts filter kind =
    Formula.interpret filter @@ function

    | Is kind' ->
      Lwt.return (Formula.interpret_bool (kind = kind'))

    | Simple ->
      (match kind with
       | _, [_] -> Lwt.return Formula.interpret_true
       | _ -> Lwt.return Formula.interpret_false)

    | Version vfilter ->
      (match kind with
       | _, [vkind] -> Version.accepts vfilter vkind
       | _ -> Lwt.return Formula.interpret_false)

  let raw string =
    match Kind.base_of_string_opt string with
    | Some bkind -> Ok (base (Base.is bkind))
    | None ->
      match Kind.version_of_string_opt string with
      | Some vkind -> Ok (version (Version.is vkind))
      | None ->
        match Kind.dance_of_string_opt string with
        | Some dkind -> Ok (is dkind)
        | None -> error_fmt "could not interpret \"%s\" as a kind for dances" string

  let nullary_text_predicates = []

  let unary_text_predicates = []

  let from_text_formula =
    TextFormula.make_to_formula raw
      nullary_text_predicates
      unary_text_predicates
end
