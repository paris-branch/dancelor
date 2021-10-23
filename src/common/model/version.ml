open Nes

let _key = "version"

type t =
  { slug : t Slug.t ;
    status : Status.t                 [@default Status.bot] ;
    tune : Tune.t Slug.t ;
    bars : int ;
    key : Music.key ;
    structure : string ;
    arranger : Credit.t Slug.t option [@default None] ;
    sources : Source.t Slug.t list    [@default []] ;
    remark : string                   [@default ""] ;
    disambiguation : string           [@default ""] }
[@@deriving yojson]

let slug t = Lwt.return t.slug
let status t = Lwt.return t.status
let tune t = Lwt.return t.tune
let bars t = Lwt.return t.bars
let key t = Lwt.return t.key
let structure t = Lwt.return t.structure
let arranger t = Lwt.return t.arranger
let sources t = Lwt.return t.sources
let remark t = Lwt.return t.remark
let disambiguation t = Lwt.return t.disambiguation

let equal version1 version2 =
  let%lwt slug1 = slug version1 in
  let%lwt slug2 = slug version2 in
  Lwt.return (Slug.equal slug1 slug2)

module Filter = struct
  let _key = "version-filter"

  type predicate =
    | Is of t
    | Tune of Tune.Filter.t
    | Key of Music.key
    | BarsEq of int | BarsNe of int
    | BarsGt of int | BarsGe of int
    | BarsLt of int | BarsLe of int
  [@@deriving yojson]

  type t = predicate Formula.t
  [@@deriving yojson]

  let is version = Formula.pred (Is version)
  let tune tfilter = Formula.pred (Tune tfilter)
  let tuneIs tune_ = tune (Tune.Filter.is tune_)
  let key key_ = Formula.pred (Key key_)
  let barsEq bars_ = Formula.pred (BarsEq bars_)
  let barsNe bars_ = Formula.pred (BarsNe bars_)
  let barsGt bars_ = Formula.pred (BarsGt bars_)
  let barsGe bars_ = Formula.pred (BarsGe bars_)
  let barsLt bars_ = Formula.pred (BarsLt bars_)
  let barsLe bars_ = Formula.pred (BarsLe bars_)

  let raw string = tune (Tune.Filter.raw string)

  let nullary_text_predicates = []

  let unary_text_predicates =
    TextFormula.[
      "tune",    (tune @@@ Tune.Filter.from_text_formula);
      "key",     raw_only ~convert:Music.key_of_string key;
      "bars",    raw_only ~convert:int_of_string barsEq;
      "bars-eq", raw_only ~convert:int_of_string barsEq;
      "bars-ne", raw_only ~convert:int_of_string barsNe;
      "bars-gt", raw_only ~convert:int_of_string barsGt;
      "bars-ge", raw_only ~convert:int_of_string barsGe;
      "bars-lt", raw_only ~convert:int_of_string barsLt;
      "bars-le", raw_only ~convert:int_of_string barsLe
    ]

  let from_text_formula =
    TextFormula.make_to_formula raw
      nullary_text_predicates
      unary_text_predicates
end
