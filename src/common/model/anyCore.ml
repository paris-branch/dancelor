open Nes

let _key = "any"

type t =
  | Credit  of  CreditCore.t
  | Dance   of   DanceCore.t
  | Person  of  PersonCore.t
  | Book    of    BookCore.t
  | Set     of     SetCore.t
  | Tune    of    TuneCore.t
  | Version of VersionCore.t
[@@deriving yojson]

let equal any1 any2 =
  match any1, any2 with
  |  Credit c1,  Credit c2 ->  CreditCore.equal c1 c2
  |   Dance d1,   Dance d2 ->   DanceCore.equal d1 d2
  |  Person p1,  Person p2 ->  PersonCore.equal p1 p2
  |    Book b1,    Book b2 ->    BookCore.equal b1 b2
  |     Set s1,     Set s2 ->     SetCore.equal s1 s2
  |    Tune t1,    Tune t2 ->    TuneCore.equal t1 t2
  | Version v1, Version v2 -> VersionCore.equal v1 v2
  | _ -> Lwt.return_false

module Type = struct
  let _key = "type"

  type t =
    | Credit
    | Dance
    | Person
    | Book
    | Set
    | Tune
    | Version
  [@@deriving yojson]

  module Set = Stdlib.Set.Make(struct
      type nonrec t = t
      let compare = compare
    end)

  let all = Set.of_list [ Credit; Dance; Person; Book; Set; Tune; Version; ]

  let equal = (=)

  let to_string = function
    | Credit  -> "Credit"
    | Dance   -> "Dance"
    | Person  -> "Person"
    | Book    -> "Book"
    | Set     -> "Set"
    | Tune    -> "Tune"
    | Version -> "Version"

  exception NotAType of string

  let of_string str =
    match String.lowercase_ascii str with
    | "credit"  -> Credit
    | "dance"   -> Dance
    | "person"  -> Person
    | "book"    -> Book
    | "set"     -> Set
    | "tune"    -> Tune
    | "version" -> Version
    | _ -> raise (NotAType str)

  let of_string_opt str =
    try Some (of_string str)
    with NotAType _ -> None
end

let type_of = function
  |  Credit _ -> Type.Credit
  |   Dance _ -> Type.Dance
  |  Person _ -> Type.Person
  |    Book _ -> Type.Book
  |     Set _ -> Type.Set
  |    Tune _ -> Type.Tune
  | Version _ -> Type.Version
