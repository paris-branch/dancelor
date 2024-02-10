open Nes

let _key = "person"

type t =
  { slug : t Slug.t ;
    status : Status.t [@default Status.bot] ;
    name : string ;
    scddb_id : int option [@default None] [@key "scddb-id"] ;
    modified_at : Datetime.t [@key "modified-at"] ;
    created_at  : Datetime.t [@key "created-at"] }
[@@deriving yojson, make, show {with_path = false}]

(* FIXME: PPX *)
let slug person = person.slug
let status person = person.status
let name person = person.name
let scddb_id person = person.scddb_id
let modified_at person = person.modified_at
let created_at person = person.created_at

module Filter = struct
  let _key = "person-filter"

  (* Dirty trick necessary to convince [ppx_deriving_qcheck] that it can
     generate a [t Slug.t]. Fine since [Slug.gen] ignores its first argument. *)
  let gen = QCheck.Gen.pure (Obj.magic 0)

  type predicate =
    | Is of t Slug.t
    | Name of string
    | NameMatches of string
  [@@deriving show {with_path = false}, qcheck, yojson]

  (* FIXME: PPX *)
  let is person = Is person
  let name name = Name name
  let nameMatches name = NameMatches name

  let unIs = function Is s -> Some s | _ -> None
  let unName = function Name n -> Some n | _ -> None
  let unNameMatches = function NameMatches n -> Some n | _ -> None

  (* FIXME: QCheck2 does this automatically. *)
  let shrink = let open QCheck in function
      | Is slug -> Iter.map is (Slug.shrink slug)
      | Name string -> Iter.map name (Shrink.string string)
      | NameMatches string -> Iter.map nameMatches (Shrink.string string)

  type t = predicate Formula.t
  [@@deriving show {with_path = false}, qcheck, yojson]

  let name' = Formula.pred % name
  let nameMatches' = Formula.pred % nameMatches

  let shrink' = Formula.shrink shrink
end
