type t =
  | Credit of Credit.t
  | Dance of Dance.t
  | Person of Person.t
  | Book of Book.t
  | Set of Set.t
  | Source of Source.t
  | Tune of Tune.t
  | Version of Version.t
[@@deriving yojson]

let _key = "any"

module Type = struct
  type t =
    | Credit
    | Dance
    | Person
    | Book
    | Set
    | Source
    | Tune
    | Version
  [@@deriving yojson]

  let to_string = function
    | Credit -> "Credit"
    | Dance -> "Dance"
    | Person -> "Person"
    | Book -> "Book"
    | Set -> "Set"
    | Source -> "Source"
    | Tune -> "Tune"
    | Version -> "Version"

  let _key = "type"
end

let type_of = function
  | Credit _ -> Type.Credit
  | Dance _ -> Type.Dance
  | Person _ -> Type.Person
  | Book _ -> Type.Book
  | Set _ -> Type.Set
  | Source _ -> Type.Source
  | Tune _ -> Type.Tune
  | Version _ -> Type.Version
