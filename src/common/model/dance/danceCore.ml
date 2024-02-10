open Nes

let _key = "dance"

type t =
  { slug : t Slug.t ;
    status : Status.t [@default Status.bot] ;
    name : string ;
    kind : Kind.Dance.t ;
    deviser : PersonCore.t Slug.t option [@default None] ;
    two_chords : bool [@default false] [@key "two-chords"] ;
    scddb_id : int option [@default None] [@key "scddb-id"] ;
    disambiguation : string [@default ""] ;
    modified_at : Datetime.t [@key "modified-at"] ;
    created_at  : Datetime.t [@key "created-at"] }
[@@deriving make, show {with_path = false}, yojson]

(* FIXME: PPX *)
let slug dance = dance.slug
let status dance = dance.status
let name dance = dance.name
let kind dance = dance.kind
let deviser dance = dance.deviser
let two_chords dance = dance.two_chords
let scddb_id dance = dance.scddb_id
let disambiguation dance = dance.disambiguation
let modified_at dance = dance.modified_at
let created_at dance = dance.created_at

module Filter = struct
  let _key = "dance-filter"

  (* Dirty trick necessary to convince [ppx_deriving_qcheck] that it can
     generate a [t Slug.t]. Fine since [Slug.gen] ignores its first argument. *)
  let gen = QCheck.Gen.pure (Obj.magic 0)
  let equal _ _ = assert false

  type predicate =
    | Is of t Slug.t
    | Name of string
    | NameMatches of string
    | Kind of Kind.Dance.Filter.t
    | Deviser of PersonCore.Filter.t (** deviser is defined and passes the filter *)
  [@@deriving eq, show {with_path = false}, qcheck, yojson]

  (* FIXME: PPX *)
  let is dance = Is dance
  let name name = Name name
  let nameMatches name = NameMatches name
  let kind kfilter = Kind kfilter
  let deviser cfilter = Deviser cfilter

  let unIs = function Is s -> Some s | _ -> None
  let unName = function Name n -> Some n | _ -> None
  let unNameMatches = function NameMatches n -> Some n | _ -> None
  let unKind = function Kind kf -> Some kf | _ -> None
  let unDeviser = function Deviser cf -> Some cf | _ -> None

  (* FIXME: QCheck2 does this automatically. *)
  let shrink =
    let open QCheck in
    let open Iter in
    function
    | Is slug -> map is (Slug.shrink slug)
    | Name string -> map name (Shrink.string string)
    | NameMatches string -> map nameMatches (Shrink.string string)
    | Deviser person -> return (Name "a") <+> map deviser (PersonCore.Filter.shrink' person)
    | Kind _ -> return (Name "a")

  type t = predicate Formula.t
  [@@deriving eq, show {with_path = false}, qcheck, yojson]

  let name' = Formula.pred % name
  let nameMatches' = Formula.pred % nameMatches
  let kind' = Formula.pred % kind
  let deviser' = Formula.pred % deviser

  let shrink' = Formula.shrink shrink
end
