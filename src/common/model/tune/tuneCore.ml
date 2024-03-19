open Nes

let _key = "tune"

type t =
  { slug : t Slug.t ;
    status : Status.t                   [@default Status.bot] ;
    name : string ;
    alternative_names : string list     [@key "alternative-names"] [@default []] ;
    kind : Kind.Base.t ;
    composers : PersonCore.t Slug.t list [@default []] ;
    dances : DanceCore.t Slug.t list    [@default []] ;
    remark : string                     [@default ""] ;
    scddb_id : int option               [@default None] [@key "scddb-id"] ;
    date : PartialDate.t option [@default None] ; (** When the tune was composed. *)
    modified_at : Datetime.t            [@key "modified-at"] ;
    created_at  : Datetime.t            [@key "created-at"] }
[@@deriving make, show {with_path = false}, yojson]

(* FIXME: PPX *)
let slug tune = tune.slug
let status tune = tune.status
let name tune = tune.name
let alternative_names tune = tune.alternative_names
let kind tune = tune.kind
let composers tune = tune.composers
let dances tune = tune.dances
let remark tune = tune.remark
let scddb_id tune = tune.scddb_id
let date tune = tune.date
let modified_at tune = tune.modified_at
let created_at tune = tune.created_at

let compare = Slug.compare_slugs_or ~fallback:Stdlib.compare slug
let equal tune1 tune2 = compare tune1 tune2 = 0

module Filter = struct
  let _key = "tune-filter"

  (* Dirty trick to convince [ppx_deriving.std] that it can derive the equality
     of [t Slug.t]. [Slug.equal] ignores its first argument anyways. *)
  let equal _ _ = assert false

  type predicate =
    | Is of t Slug.t
    | Name of string
    | NameMatches of string
    | ExistsComposer of PersonCore.Filter.t (** one of the composers of the list passes the filter *)
    | Kind of Kind.Base.Filter.t
    | ExistsDance of DanceCore.Filter.t
  [@@deriving eq, show {with_path = false}, yojson, variants]

  type t = predicate Formula.t
  [@@deriving eq, show {with_path = false}, yojson]

  let name' = Formula.pred % name
  let nameMatches' = Formula.pred % nameMatches
  let existsComposer' = Formula.pred % existsComposer
  let kind' = Formula.pred % kind
  let existsDance' = Formula.pred % existsDance
end
