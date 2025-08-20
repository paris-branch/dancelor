open Nes

include KindDanceType

let rec pp ppf = function
  | Version version_kind -> fpf ppf "%s" (KindVersion.to_string version_kind)
  | Add (kind1, kind2) -> fpf ppf "%a + %a" pp kind1 pp kind2
  | Mul (n, kind) ->
    (match kind with Add _ -> fpf ppf "%d x (%a)" | _ -> fpf ppf "%d x %a")
      n
      pp
      kind

let to_string kind =
  Format.with_formatter_to_string (fun ppf -> pp ppf kind)

let of_string_opt s =
  try
    Some (KindDanceParser.main KindDanceLexer.token (Lexing.from_string s))
  with
    | KindDanceParser.Error | KindDanceLexer.UnexpectedCharacter _ -> None

let of_string s =
  match of_string_opt s with Some k -> k | None -> failwith "Kind.Dance.of_string"

let%test _ = to_string (Mul (3, Version (32, Strathspey))) = "3 x 32 S"
let%test _ = to_string (Version (128, Jig)) = "128 J"
let%test _ = to_string (Mul (2, Add (Version (32, Strathspey), Version (24, Reel)))) = "2 x (32 S + 24 R)"
let%test _ = to_string (Add (Version (32, Strathspey), Add (Mul (7, Add (Version (12, Jig), Version (24, Reel))), Version (888, Strathspey)))) = "32 S + 7 x (12 J + 24 R) + 888 S"

let%test _ = of_string "3 x ( 32 Strathspey )" = Mul (3, Version (32, Strathspey))
let%test _ = of_string "(32W + 64R)" = Add (Version (32, Waltz), Version (64, Reel))
let%test _ = of_string "3x40J" = Mul (3, Version (40, Jig))
let%test _ = of_string "32R" = Version (32, Reel)
let%test _ = of_string_opt "R" = None

let to_yojson d =
  `String (to_string d)

let of_yojson = function
  | `String s ->
    (
      try
        Ok (of_string s)
      with
        | _ -> Error "Dancelor_common.Model.Kind.of_yojson: not a valid dance kind"
    )
  | _ -> Error "Dancelor_common.Model.Kind.of_yojson: not a JSON string"

let rec to_pretty_string = function
  | Version vkind -> KindVersion.to_pretty_string vkind
  | Add (kind1, kind2) -> spf "%s + %s" (to_pretty_string kind1) (to_pretty_string kind2)
  | Mul (n, kind) ->
    (match kind with Add _ -> spf "%d x (%s)" | _ -> spf "%d x %s")
      n
      (to_pretty_string kind)

let rec version_kinds = function
  | Version vkind -> [vkind]
  | Add (kind1, kind2) -> version_kinds kind1 @ version_kinds kind2
  | Mul (_n, kind) -> version_kinds kind

let rec to_simple = function
  | Version (bars, base) -> Some (1, bars, base)
  | Add (kind1, kind2) ->
    Option.bind (to_simple kind1) @@ fun (n1, bars1, base1) ->
    Option.bind (to_simple kind2) @@ fun (n2, bars2, base2) ->
    if base1 = base2 then
      if bars1 = bars2 then
        Some (n1 + n2, bars1, base1)
      else
        Some (1, n1 * bars1 + n2 * bars2, base1)
    else
      None
  | Mul (n, kind) ->
    Option.map (fun (n', bars, base) -> (n * n', bars, base)) (to_simple kind)

(* Filters *)

module Filter = struct
  type predicate =
    | Is of t
    | Simple
    | Version of KindVersion.Filter.t
  [@@deriving eq, show {with_path = false}, yojson, variants]

  let base = version % KindVersion.Filter.base'
  let baseIs = version % KindVersion.Filter.baseIs'

  type t = predicate Formula.t
  [@@deriving eq, show {with_path = false}, yojson]

  let is' = Formula.pred % is
  let version' = Formula.pred % version
  let simple' = Formula.pred simple
  let base' = Formula.pred % base
  let baseIs' = Formula.pred % baseIs

  let accepts filter kind =
    Formula.interpret filter @@ function
      | Is kind' ->
        lwt (Formula.interpret_bool (kind = kind'))
      | Simple ->
        (
          match kind with
          | Mul (_, Version _) -> lwt Formula.interpret_true
          | _ -> lwt Formula.interpret_false
        )
      | Version vfilter ->
        Formula.interpret_and_l
        <$> Lwt_list.map_s
            (KindVersion.Filter.accepts vfilter)
            (version_kinds kind)

  let text_formula_converter =
    TextFormulaConverter.(
      merge
        ~tiebreaker: Left
        (
          (* Dance kind-specific converter *)
          make
            [
              raw
                (fun string ->
                  Option.fold
                    ~some: (ok % is')
                    ~none: (kspf error "could not interpret \"%s\" as a dance kind" string)
                    (of_string_opt string)
                );
              nullary ~name: "simple" Simple;
              unary_lift ~name: "version" (version, version_val) ~converter: KindVersion.Filter.text_formula_converter;
              unary_raw ~wrap_back: Never ~name: "is" (is, is_val) ~cast: (of_string_opt, to_pretty_string) ~type_: "dance kind";
            ]
        )
        (
          (* Version kind converter, lifted to dance kinds; lose in case of tiebreak. *)
          map version KindVersion.Filter.text_formula_converter
        )
    )

  let from_text_formula = TextFormula.to_formula text_formula_converter
  let from_string ?filename input =
    Result.bind (TextFormula.from_string ?filename input) from_text_formula

  (* Little trick to convince OCaml that polymorphism is OK. *)
  type op = {op: 'a. 'a Formula.t -> 'a Formula.t -> 'a Formula.t}

  let optimise =
    let lift {op} f1 f2 =
      match (f1, f2) with
      | (Version f1, Version f2) -> some @@ version (op f1 f2)
      | _ -> None
    in
    Formula.optimise
      ~lift_and: (lift {op = Formula.and_})
      ~lift_or: (lift {op = Formula.or_})
      (function
        | (Is _ as p) | (Simple as p) -> p
        | Version vfilter -> version @@ KindVersion.Filter.optimise vfilter
      )
end
