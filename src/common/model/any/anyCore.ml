let _key = "any"

type t =
  | Credit  of  CreditCore.t
  | Dance   of   DanceCore.t
  | Book    of    BookCore.t
  | Set     of     SetCore.t
  | Tune    of    TuneCore.t
  | Version of VersionCore.t
[@@deriving yojson]

module Type = struct
  let _key = "type"

  type t =
    | Credit
    | Dance
    | Book
    | Set
    | Tune
    | Version
  [@@deriving yojson]
end

module Filter = struct
  let _key = "any-filter"

  type predicate =
    | Type of Type.t
    (* lifting predicates: *)
    | AsCredit  of  CreditCore.Filter.t
    | AsDance   of   DanceCore.Filter.t
    | AsBook    of    BookCore.Filter.t
    | AsSet     of     SetCore.Filter.t
    | AsTune    of    TuneCore.Filter.t
    | AsVersion of VersionCore.Filter.t
  [@@deriving yojson]

  type t = predicate Formula.t
  [@@deriving yojson]
end
