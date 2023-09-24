open Nes

let _key = "kind-dance"

include KindDanceType

let rec pp ppf = function
  | Version version_kind -> fpf ppf "%s" (KindVersion.to_string version_kind)
  | Add (kind1, kind2) -> fpf ppf "%a + %a" pp kind1 pp kind2
  | Mul (n, kind) ->
    (match kind with Add _ -> fpf ppf "%d x (%a)" | _ -> fpf ppf "%d x %a")
      n pp kind


let check s =
  match of_string s with
  | exception (Invalid_argument _) -> false
  | exception (Scanf.Scan_failure _) -> false
  | exception (End_of_file) -> false
  | _ -> true

let%test _ = to_string (3, [32, Strathspey]) = "3 x 32 S"
let%test _ = to_string (1, [128, Jig]) = "128 J"
let%test _ = to_string (2, [(32, Strathspey); (24, Reel)]) = "2 x (32 S + 24 R)"

let%test _ = of_string "3 x ( 32 Strathspey )" = (3, [32, Strathspey])
let%test _ = of_string "(32W + 64R)" = (1, [(32, Waltz); (64, Reel)])
let%test _ = of_string "3x40J" = (3, [40, Jig])
let%test _ = of_string "32R" = (1, [32, Reel])
let%test _ =
  try ignore (of_string "R"); false
  with Invalid_argument _ -> true
let to_string kind =
  Format.with_formatter_to_string (fun ppf -> pp ppf kind)

let of_string_opt s =
  try Some (KindDanceParser.main KindDanceLexer.token (Lexing.from_string s))
  with KindDanceParser.Error -> None

let of_string s =
  match of_string_opt s with Some k -> k | None -> failwith "Kind.Dance.of_string"


let to_yojson d =
  `String (to_string d)

let of_yojson = function
  | `String s ->
    (try Ok (of_string s)
     with _ -> Error "Dancelor_common_model.Kind.of_yojson: not a valid dance kind")
  | _ -> Error "Dancelor_common_model.Kind.of_yojson: not a JSON string"

let to_pretty_string (repeats, versions) =
  versions
  |> List.map KindVersion.to_pretty_string
  |> String.concat " + "
  |> (if repeats = 1 || List.length versions = 1 then Fun.id else spf "(%s)")
  |> (if repeats = 1 then Fun.id else spf "%d x %s" repeats)

(* Filters *)

type dance_kind = t (* needed for signature of filters *)

module Filter = struct
  type predicate =
    | Is of t
    | Simple
    | Version of KindVersion.Filter.t
  [@@deriving yojson]

  type t = predicate Formula.t
  [@@deriving yojson]

  let is kind = Formula.pred (Is kind)
  let version vfilter = Formula.pred (Version vfilter)
  let base bfilter = version (KindVersion.Filter.base bfilter)

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
       | _, [vkind] -> KindVersion.Filter.accepts vfilter vkind
       | _ -> Lwt.return Formula.interpret_false)

  let raw string =
    match KindBase.of_string_opt string with
    | Some bkind -> Ok (base (KindBase.Filter.is bkind))
    | None ->
      match KindVersion.of_string_opt string with
      | Some vkind -> Ok (version (KindVersion.Filter.is vkind))
      | None ->
        match of_string_opt string with
        | Some dkind -> Ok (is dkind)
        | None -> error_fmt "could not interpret \"%s\" as a kind for dances" string

  let nullary_text_predicates = []

  (* Unary text_predicates lifted from Versions. *)
  let unary_text_predicates =
    List.map
      (fun (name, builder) ->
         (name,
          (fun formula ->
             match builder formula with
             | Ok formula -> Ok (version formula)
             | Error err -> Error err)))
      KindVersion.Filter.unary_text_predicates

  let from_text_formula =
    TextFormula.make_to_formula raw
      nullary_text_predicates
      unary_text_predicates
end
