let _key = "any"

type t =
  | Person  of  PersonCore.t
  | Dance   of   DanceCore.t
  | Book    of    BookCore.t
  | Set     of     SetCore.t
  | Tune    of    TuneCore.t
  | Version of VersionCore.t
[@@deriving show {with_path = false}, yojson]

(* FIXME: PPX *)
let person p = Person p
let dance d = Dance d
let book b = Book b
let set s = Set s
let tune t = Tune t
let version v = Version v

module Type = struct
  let _key = "type"

  type t =
    | Person
    | Dance
    | Book
    | Set
    | Tune
    | Version
  [@@deriving show {with_path = false}, yojson]
end

module Filter = struct
  let _key = "any-filter"

  type predicate =
    | Type of Type.t
    (* lifting predicates: *)
    | AsPerson  of  PersonCore.Filter.t
    | AsDance   of   DanceCore.Filter.t
    | AsBook    of    BookCore.Filter.t
    | AsSet     of     SetCore.Filter.t
    | AsTune    of    TuneCore.Filter.t
    | AsVersion of VersionCore.Filter.t
  [@@deriving show {with_path = false}, yojson]

  (* FIXME: PPX *)
  let type_ type_ = Type type_
  let asPerson  filter = AsPerson  filter
  let asDance   filter = AsDance   filter
  let asBook    filter = AsBook    filter
  let asSet     filter = AsSet     filter
  let asTune    filter = AsTune    filter
  let asVersion filter = AsVersion filter

  let unType = function Type t -> Some t | _ -> None
  let unAsPerson = function AsPerson pf -> Some pf | _ -> None
  let unAsDance = function AsDance df -> Some df | _ -> None
  let unAsBook = function AsBook bf -> Some bf | _ -> None
  let unAsSet = function AsSet sf -> Some sf | _ -> None
  let unAsTune = function AsTune tf -> Some tf | _ -> None
  let unAsVersion = function AsVersion vf -> Some vf | _ -> None

  type t = predicate Formula.t
  [@@deriving show {with_path = false}, yojson]
end
