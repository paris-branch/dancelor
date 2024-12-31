open Nes

let _key = "any"

type t =
  | Person of PersonCore.t
  | Dance of DanceCore.t
  | Book of BookCore.t
  | Set of SetCore.t
  | Tune of TuneCore.t
  | Version of VersionCore.t
[@@deriving show {with_path = false}, yojson, variants]

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

  let compare t1 t2 =
    let to_int = function
      | Person -> 0
      | Dance -> 1
      | Tune -> 2
      | Version -> 3
      | Set -> 4
      | Book -> 5
    in
    if t1 = t2 then
      0
    else
      Int.compare (to_int t1) (to_int t2)
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
    | Person of PersonCore.Filter.t
    | Dance of DanceCore.Filter.t
    | Book of BookCore.Filter.t
    | Set of SetCore.Filter.t
    | Tune of TuneCore.Filter.t
    | Version of VersionCore.Filter.t
  [@@deriving eq, show {with_path = false}, yojson, variants]

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
