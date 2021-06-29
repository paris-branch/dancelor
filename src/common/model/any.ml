let _key = "any"

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

let equal any1 any2 =
  match any1, any2 with
  | Credit c1,  Credit c2  -> Credit.equal  c1 c2
  | Dance d1,   Dance d2   -> Dance.equal   d1 d2
  | Person p1,  Person p2  -> Person.equal  p1 p2
  | Book b1,    Book b2    -> Book.equal    b1 b2
  | Set s1,     Set s2     -> Set.equal     s1 s2
  | Source s1,  Source s2  -> Source.equal  s1 s2
  | Tune t1,    Tune t2    -> Tune.equal    t1 t2
  | Version v1, Version v2 -> Version.equal v1 v2
  | _ -> Lwt.return_false

module Type = struct
  let _key = "type"

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

  let equal = (=)

  let to_string = function
    | Credit -> "Credit"
    | Dance -> "Dance"
    | Person -> "Person"
    | Book -> "Book"
    | Set -> "Set"
    | Source -> "Source"
    | Tune -> "Tune"
    | Version -> "Version"
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

module Filter = struct
  let _key = "any-filter"

  type any = t
  [@@deriving yojson]

  type t =
    | Is of any
    | TypeIs of Type.t
  [@@deriving yojson]
end
