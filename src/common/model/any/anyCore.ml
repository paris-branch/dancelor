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
    | Person  of  PersonCore.Filter.t
    | Dance   of   DanceCore.Filter.t
    | Book    of    BookCore.Filter.t
    | Set     of     SetCore.Filter.t
    | Tune    of    TuneCore.Filter.t
    | Version of VersionCore.Filter.t
  [@@deriving show {with_path = false}, yojson]

  (* FIXME: PPX *)
  let type_ type_ = Type type_
  let person  filter = Person  filter
  let dance   filter = Dance   filter
  let book    filter = Book    filter
  let set     filter = Set     filter
  let tune    filter = Tune    filter
  let version filter = Version filter

  let unType = function Type t -> Some t | _ -> None
  let unPerson = function Person pf -> Some pf | _ -> None
  let unDance = function Dance df -> Some df | _ -> None
  let unBook = function Book bf -> Some bf | _ -> None
  let unSet = function Set sf -> Some sf | _ -> None
  let unTune = function Tune tf -> Some tf | _ -> None
  let unVersion = function Version vf -> Some vf | _ -> None

  type t = predicate Formula.t
  [@@deriving show {with_path = false}, yojson]
end
