open Nes

let _key = "set"

type t =
  { slug : t Slug.t                  [@default Slug.none] ;
    status : Status.t                [@default Status.bot] ;
    name : string ;
    conceptors : PersonCore.t Slug.t list [@default []] ;
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
let conceptors set = set.conceptors
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

  (* Dirty trick to convince [ppx_deriving.std] that it can derive the equality
     of [t Slug.t]. [Slug.equal] ignores its first argument anyways. *)
  let equal _ _ = assert false

  type predicate =
    | Is of t Slug.t
    | Name of string
    | NameMatches of string
    | ExistsConceptor of PersonCore.Filter.t (** conceptor is defined and passes the filter *)
    | ExistsVersion of VersionCore.Filter.t
    | Kind of Kind.Dance.Filter.t
  [@@deriving eq, show {with_path = false}, yojson, variants]

  type t = predicate Formula.t
  [@@deriving eq, show {with_path = false}, yojson]

  let name' = Formula.pred % name
  let nameMatches' = Formula.pred % nameMatches
  let existsConceptor' = Formula.pred % existsConceptor
  let existsVersion' = Formula.pred % existsVersion
  let kind' = Formula.pred % kind
end
