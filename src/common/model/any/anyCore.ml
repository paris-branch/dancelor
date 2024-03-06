open Nes

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
  [@@deriving eq, show {with_path = false}, yojson]
end

module Filter = struct
  let _key = "any-filter"

  (* NOTE: This [Raw] variant is a bit artificial, when we could already be
     inheriting the various [raw] cases, of the other filters. However, this
     would unfold text formulas into a big disjunction at the syntactic level,
     and we would rather avoid that. *)
  type predicate =
    | Raw of string
    | Type of Type.t
    (* lifting predicates: *)
    | Person  of  PersonCore.Filter.t
    | Dance   of   DanceCore.Filter.t
    | Book    of    BookCore.Filter.t
    | Set     of     SetCore.Filter.t
    | Tune    of    TuneCore.Filter.t
    | Version of VersionCore.Filter.t
  [@@deriving eq, show {with_path = false}, yojson]

  (* FIXME: PPX *)
  let raw string = Raw string
  let type_ type_ = Type type_
  let person  filter = Person  filter
  let dance   filter = Dance   filter
  let book    filter = Book    filter
  let set     filter = Set     filter
  let tune    filter = Tune    filter
  let version filter = Version filter

  let unRaw = function Raw s -> Some s | _ -> None
  let unType = function Type t -> Some t | _ -> None
  let unPerson = function Person pf -> Some pf | _ -> None
  let unDance = function Dance df -> Some df | _ -> None
  let unBook = function Book bf -> Some bf | _ -> None
  let unSet = function Set sf -> Some sf | _ -> None
  let unTune = function Tune tf -> Some tf | _ -> None
  let unVersion = function Version vf -> Some vf | _ -> None

  type t = predicate Formula.t
  [@@deriving eq, show {with_path = false}, yojson]

  let raw' = Formula.pred % raw
  let type_' = Formula.pred % type_
  let person' = Formula.pred % person
  let dance' = Formula.pred % dance
  let book' = Formula.pred % book
  let set' = Formula.pred % set
  let tune' = Formula.pred % tune
  let version' = Formula.pred % version
end
