open Nes

let _key = "version"

type t =
  { slug : t Slug.t ;
    status : Status.t                 [@default Status.bot] ;
    tune : TuneCore.t Slug.t ;
    bars : int ;
    key : Music.key ;
    structure : string ;
    sources : string list [@default []] ; (* FIXME: remove from DB *)
    arranger : PersonCore.t Slug.t option [@default None] ;
    remark : string                   [@default ""] ;
    disambiguation : string           [@default ""] ;
    broken : bool                     [@default false] ;
    modified_at : Datetime.t          [@key "modified-at"] ;
    created_at  : Datetime.t          [@key "created-at"] }
[@@deriving make, show {with_path = false}, yojson]

(* FIXME: PPX *)
let slug version = version.slug
let status version = version.status
let tune version = version.tune
let bars version = version.bars
let key version = version.key
let structure version = version.structure
let sources version = version.sources
let arranger version = version.arranger
let remark version = version.remark
let disambiguation version = version.disambiguation
let broken version = version.broken
let modified_at version = version.modified_at
let created_at  version = version.created_at

let equal version1 version2 = Slug.equal' (slug version1) (slug version2)

module Filter = struct
  let _key = "version-filter"

  (* Dirty trick necessary to convince [ppx_deriving_qcheck] that it can
     generate a [t Slug.t]. Fine since [Slug.gen] ignores its first argument. *)
  let gen = QCheck.Gen.pure (Obj.magic 0)

  type predicate =
    | Is of t Slug.t
    | Tune of TuneCore.Filter.t
    | Key of Music.key
    | Kind of Kind.Version.Filter.t
    | Broken
  [@@deriving eq, show {with_path = false}, qcheck, yojson]

  (* FIXME: PPX *)
  let is version = Is version
  let tune tfilter = Tune tfilter
  let key key_ = Key key_
  let kind kfilter = Kind kfilter
  let broken = Broken

  let unIs = function Is v -> Some v | _ -> None
  let unTune = function Tune f -> Some f | _ -> None
  let unKey = function Key k -> Some k | _ -> None
  let unKind = function Kind f -> Some f | _ -> None

  (* FIXME: QCheck2 does this automatically. *)
  let shrink = let open QCheck.Iter in function
      | Broken -> empty
      | Is slug -> return Broken <+> map is (Slug.shrink slug)
      | Tune tf -> return Broken <+> map tune (TuneCore.Filter.shrink' tf)
      | Kind kf -> return Broken <+> map kind (Kind.Version.Filter.shrink' kf)
      | Key _ -> return Broken

  type t = predicate Formula.t
  [@@deriving eq, show {with_path = false}, qcheck, yojson]

  let tune' = Formula.pred % tune
  let key' = Formula.pred % key
  let kind' = Formula.pred % kind
  let broken' = Formula.pred broken

  let shrink' = Formula.shrink shrink
end
