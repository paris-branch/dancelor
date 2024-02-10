open Nes

module PageCore = struct
  let _key = "book-page"

  type t =
    | Version       of VersionCore.t Slug.t * VersionParameters.t
    | Set           of     SetCore.t Slug.t * SetParameters.t
    | InlineSet     of     SetCore.t        * SetParameters.t
  [@@deriving show {with_path = false}, yojson]
end

let _key = "book"

type t =
  { slug        : t Slug.t ;
    status      : Status.t   [@default Status.bot] ;
    title       : string ;
    subtitle    : string     [@default ""] ;
    short_title : string     [@default ""] [@key "short-title"] ;
    date        : PartialDate.t option [@default None] ;
    contents    : PageCore.t list ;
    source      : bool       [@default false] ;
    remark      : string     [@default ""] ;
    scddb_id    : int option [@default None] [@key "scddb-id"] ;
    modified_at : Datetime.t [@key "modified-at"] ;
    created_at  : Datetime.t [@key "created-at"] }
[@@deriving make, show {with_path = false}, yojson]

let make
    ~slug ?status ~title ?subtitle ?short_title ?date ?(contents=[]) ?source ?remark
    ?scddb_id ~modified_at ~created_at
    ()
  =
  make
    ~slug ?status ~title ?subtitle ?short_title ~date ~contents ?source ?remark
    ~scddb_id ~modified_at ~created_at
    ()

(* FIXME: PPX *)
let slug book = book.slug
let status book = book.status
let title book = book.title
let subtitle book = book.subtitle
let short_title book = book.short_title
let date book = book.date
let contents book = book.contents
let source book = book.source
let remark book = book.remark
let scddb_id book = book.scddb_id
let modified_at book = book.modified_at
let created_at book = book.created_at

let contains_set set1 book =
  List.exists
    (function
      | PageCore.Set (set2, _) -> Slug.equal set1 set2
      | _ -> false)
    book.contents

type warning =
  | Empty
  | DuplicateSet of SetCore.t (* FIXME: duplicate dance? *)
  | DuplicateVersion of TuneCore.t * (SetCore.t option * int) list
  (* DuplicateVersion contains the list of sets in which the tune appears, as
     well as the number of times this set is present *)
  | SetDanceMismatch of SetCore.t * DanceCore.t
  (* SetDanceMismatch contains a set where one of the associated dances
     does not have the same kind *)
[@@deriving show {with_path = false}, yojson]

type warnings = warning list
[@@deriving show {with_path = false}, yojson]

type page =
  | Version   of VersionCore.t * VersionParameters.t
  | Set       of     SetCore.t * SetParameters.t
  | InlineSet of     SetCore.t * SetParameters.t
[@@deriving show {with_path = false}]

module Filter = struct
  let _key = "book-filter"

  (* Dirty trick necessary to convince [ppx_deriving_qcheck] that it can
     generate a [t Slug.t]. Fine since [Slug.gen] ignores its first argument. *)
  let gen = QCheck.Gen.pure (Obj.magic 0)

  type predicate =
    | Is of t Slug.t
    | IsSource
    | Title of string
    | TitleMatches of string
    | Subtitle of string
    | SubtitleMatches of string
    | ExistsVersion of VersionCore.Filter.t
    | ExistsSet of SetCore.Filter.t
    | ExistsInlineSet of SetCore.Filter.t
    | ExistsVersionDeep of VersionCore.Filter.t
  [@@deriving show {with_path = false}, qcheck, yojson]

  (* FIXME: PPX *)
  let is book = Is book
  let title string = Title string
  let titleMatches string = TitleMatches string
  let subtitle string = Subtitle string
  let subtitleMatches string = SubtitleMatches string
  let isSource = IsSource
  let existsVersion vfilter = ExistsVersion vfilter
  let existsSet sfilter = ExistsSet sfilter
  let existsInlineSet sfilter = ExistsInlineSet sfilter
  let existsVersionDeep vfilter = ExistsVersionDeep vfilter

  let unIs = function Is s -> Some s | _ -> None
  let unTitle = function Title t -> Some t | _ -> None
  let unTitleMatches = function TitleMatches t -> Some t | _ -> None
  let unSubtitle = function Subtitle s -> Some s | _ -> None
  let unSubtitleMatches = function SubtitleMatches s -> Some s | _ -> None
  let unExistsVersion = function ExistsVersion vf -> Some vf | _ -> None
  let unExistsSet = function ExistsSet sf -> Some sf | _ -> None
  let unExistsInlineSet = function ExistsInlineSet sf -> Some sf | _ -> None
  let unExistsVersionDeep = function ExistsVersionDeep vf -> Some vf | _ -> None

  (* FIXME: QCheck2 does this automatically. *)
  let shrink =
    let open QCheck in
    let open Iter in
    function
    | IsSource -> empty
    | Is slug -> return IsSource <+> map is (Slug.shrink slug)
    | Title string -> return IsSource <+> map title (Shrink.string string)
    | TitleMatches string -> return IsSource <+> map titleMatches (Shrink.string string)
    | Subtitle string -> return IsSource <+> map subtitle (Shrink.string string)
    | SubtitleMatches string -> return IsSource <+> map subtitleMatches (Shrink.string string)
    | ExistsVersion vf -> return IsSource <+> map existsVersion (VersionCore.Filter.shrink' vf)
    | ExistsSet sf -> return IsSource <+> map existsSet (SetCore.Filter.shrink' sf)
    | ExistsInlineSet sf -> return IsSource <+> map existsInlineSet (SetCore.Filter.shrink' sf)
    | ExistsVersionDeep vf -> return IsSource <+> map existsVersionDeep (VersionCore.Filter.shrink' vf)
  [@@deriving show {with_path = false}, qcheck, yojson]

  type t = predicate Formula.t
  [@@deriving show {with_path = false}, qcheck, yojson]

  let title' = Formula.pred % title
  let titleMatches' = Formula.pred % titleMatches
  let subtitle' = Formula.pred % subtitle
  let subtitleMatches' = Formula.pred % subtitleMatches
  let isSource' = Formula.pred IsSource
  let existsVersion' = Formula.pred % existsVersion
  let existsSet' = Formula.pred % existsSet
  let existsInlineSet' = Formula.pred % existsInlineSet
  let existsVersionDeep' = Formula.pred % existsVersionDeep

  let shrink' = Formula.shrink shrink
end
