open Nes

let _key = "set"

type t =
  { slug : t Slug.t                  [@default Slug.none] ;
    status : Status.t                [@default Status.bot] ;
    name : string ;
    deviser : CreditCore.t Slug.t option [@default None] ;
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

let slug set = Lwt.return set.slug
let status set = Lwt.return set.status
let dances set = Lwt.return set.dances
let deviser set = Lwt.return set.deviser
let versions_and_parameters set = Lwt.return set.versions_and_parameters

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
    | Deviser of CreditCore.Filter.t (** deviser is defined and passes the filter *)
    | ExistsVersion of VersionCore.Filter.t
    | Kind of KindFilter.Dance.t
  [@@deriving yojson]

  type t = predicate Formula.t
  [@@deriving yojson]
end
