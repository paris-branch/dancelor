open Nes
open Dancelor_common_database
open Dancelor_common_model_utils

type t =
  | Person of Person.t Entry.t
  | Dance of Dance.t Entry.t
  | Book of Book.t Entry.t
  | Set of Set.t Entry.t
  | Tune of Tune.t Entry.t
  | Version of Version.t Entry.t
[@@deriving show {with_path = false}, yojson, variants]

module Type = struct
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
  (* NOTE: This [Raw] variant is a bit artificial, when we could already be
     inheriting the various [raw] cases, of the other filters. However, this
     would unfold text formulas into a big disjunction at the syntactic level,
     and we would rather avoid that. *)
  type predicate =
    | Raw of string
    | Type of Type.t
    (* lifting predicates: *)
    | Person of Person.Filter.t
    | Dance of Dance.Filter.t
    | Book of Book.Filter.t
    | Set of Set.Filter.t
    | Tune of Tune.Filter.t
    | Version of Version.Filter.t
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
