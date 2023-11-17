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
[@@deriving make, yojson]

(* FIXME: rename [versions_and_parameters] into [contents]. *)

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

  type predicate =
    | Is of t
    | Name of string
    | NameMatches of string
    | Deviser of PersonCore.Filter.t (** deviser is defined and passes the filter *)
    | ExistsVersion of VersionCore.Filter.t
    | Kind of Kind.Dance.Filter.t
  [@@deriving yojson]

  type t = predicate Formula.t
  [@@deriving yojson]
end
