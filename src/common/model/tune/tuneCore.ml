open Nes

let _key = "tune"

type t =
  { slug : t Slug.t ;
    status : Status.t                   [@default Status.bot] ;
    name : string ;
    alternative_names : string list     [@key "alternative-names"] [@default []] ;
    kind : Kind.Base.t ;
    author : PersonCore.t Slug.t option [@default None] ;
    dances : DanceCore.t Slug.t list    [@default []] ;
    remark : string                     [@default ""] ;
    scddb_id : int option               [@default None] [@key "scddb-id"] ;
    modified_at : Datetime.t            [@key "modified-at"] ;
    created_at  : Datetime.t            [@key "created-at"] }
[@@deriving make, show {with_path = false}, yojson]

(* FIXME: PPX *)
let slug tune = tune.slug
let status tune = tune.status
let name tune = tune.name
let alternative_names tune = tune.alternative_names
let kind tune = tune.kind
let author tune = tune.author
let dances tune = tune.dances
let remark tune = tune.remark
let scddb_id tune = tune.scddb_id
let modified_at tune = tune.modified_at
let created_at tune = tune.created_at

let compare = Slug.compare_slugs_or ~fallback:Stdlib.compare slug
let equal tune1 tune2 = compare tune1 tune2 = 0

module Filter = struct
  let _key = "tune-filter"

  (* Dirty trick necessary to convince [ppx_deriving_qcheck] that it can
     generate a [t Slug.t]. Fine since [Slug.gen] ignores its first argument. *)
  let gen = QCheck.Gen.pure (Obj.magic 0)
  let equal _ _ = assert false

  type predicate =
    | Is of t Slug.t
    | Name of string
    | NameMatches of string
    | Author of PersonCore.Filter.t (** author is defined and passes the filter *)
    | Kind of Kind.Base.Filter.t
    | ExistsDance of DanceCore.Filter.t
  [@@deriving eq, show {with_path = false}, qcheck, yojson]

  (* FIXME: PPX *)
  let is tune = Is tune
  let name string = Name string
  let nameMatches string = NameMatches string
  let author cfilter = Author cfilter
  let kind kfilter = Kind kfilter
  let existsDance dfilter = ExistsDance dfilter

  let unIs = function Is s -> Some s | _ -> None
  let unName = function Name n -> Some n | _ -> None
  let unNameMatches = function NameMatches n -> Some n | _ -> None
  let unAuthor = function Author cf -> Some cf | _ -> None
  let unKind = function Kind kf -> Some kf | _ -> None
  let unExistsDance = function ExistsDance df -> Some df | _ -> None

  (* FIXME: QCheck2 does this automatically. *)
  let shrink = let open QCheck in function
      | Is slug -> Iter.map is (Slug.shrink slug)
      | Name string -> Iter.map name (Shrink.string string)
      | NameMatches string -> Iter.map nameMatches (Shrink.string string)
      | _ -> Iter.empty

  type t = predicate Formula.t
  [@@deriving eq, show {with_path = false}, qcheck, yojson]

  let name' = Formula.pred % name
  let nameMatches' = Formula.pred % nameMatches
  let author' = Formula.pred % author
  let kind' = Formula.pred % kind
  let existsDance' = Formula.pred % existsDance

  let shrink' = Formula.shrink shrink
end
