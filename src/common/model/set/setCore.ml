open Nes

let _key = "set"

type t =
  { slug : t Slug.t                  [@default Slug.none] ;
    status : Status.t                [@default Status.bot] ;
    name : string ;
    deviser : PersonCore.t Slug.t option [@default None] ;
    kind : Kind.Dance.t ;
    versions_and_parameters : (VersionCore.t Slug.t * VersionParameters.t) list
                              [@key "versions-and-parameters"] [@default []] ;
    order : SetOrder.t ;
    instructions : string            [@default ""] ;
    dances : DanceCore.t Slug.t list [@default []] ;
    remark : string                  [@default ""] ;
    modified_at : Datetime.t      [@key "modified-at"] ;
    created_at  : Datetime.t      [@key "created-at"] }
[@@deriving make, show {with_path = false}, yojson]

(* FIXME: rename [versions_and_parameters] into [contents]. *)

(* FIXME: PPX *)
let slug set = set.slug
let status set = set.status
let name set = set.name
let deviser set = set.deviser
let kind set = set.kind
let versions_and_parameters set = set.versions_and_parameters
let order set = set.order
let instructions set = set.instructions
let dances set = set.dances
let remark set = set.remark
let modified_at set = set.modified_at
let created_at set = set.created_at

type warning =
  | Empty
  | WrongKind
  | WrongVersionBars of VersionCore.t
  | WrongVersionKind of TuneCore.t
  | DuplicateVersion of TuneCore.t
[@@deriving yojson]

type warnings = warning list
[@@deriving yojson]

module Filter = struct
  let _key = "set-filter"

  (* Dirty trick necessary to convince [ppx_deriving_qcheck] that it can
     generate a [t Slug.t]. Fine since [Slug.gen] ignores its first argument. *)
  let gen = QCheck.Gen.pure (Obj.magic 0)
  let equal _ _ = assert false

  type predicate =
    | Is of t Slug.t
    | Name of string
    | NameMatches of string
    | Deviser of PersonCore.Filter.t (** deviser is defined and passes the filter *)
    | ExistsVersion of VersionCore.Filter.t
    | Kind of Kind.Dance.Filter.t
  [@@deriving eq, show {with_path = false}, qcheck, yojson]

  (* FIXME: PPX *)
  let is set = Is set
  let name name = Name name
  let nameMatches name = NameMatches name
  let deviser cfilter = Deviser cfilter
  let existsVersion vfilter = ExistsVersion vfilter
  let kind kfilter = Kind kfilter

  let unIs = function Is s -> Some s | _ -> None
  let unName = function Name n -> Some n | _ -> None
  let unNameMatches = function NameMatches n -> Some n | _ -> None
  let unDeviser = function Deviser cf -> Some cf | _ -> None
  let unExistsVersion = function ExistsVersion vf -> Some vf | _ -> None
  let unKind = function Kind kf -> Some kf | _ -> None

  (* FIXME: QCheck2 does this automatically. *)
  let shrink = let open QCheck.Iter in function
      | Name string -> map name (QCheck.Shrink.string string)
      | Is slug -> return (Name "a") <+> map is (Slug.shrink slug)
      | NameMatches string -> return (Name "a") <+> map nameMatches (QCheck.Shrink.string string)
      | Deviser pf -> return (Name "a") <+> map deviser (PersonCore.Filter.shrink' pf)
      | ExistsVersion vf -> return (Name "a") <+> map existsVersion (VersionCore.Filter.shrink' vf)
      | Kind kf -> return (Name "a") <+> map kind (Kind.Dance.Filter.shrink' kf)

  type t = predicate Formula.t
  [@@deriving eq, show {with_path = false}, qcheck, yojson]

  let name' = Formula.pred % name
  let nameMatches' = Formula.pred % nameMatches
  let deviser' = Formula.pred % deviser
  let existsVersion' = Formula.pred % existsVersion
  let kind' = Formula.pred % kind

  let shrink' = Formula.shrink shrink
end
