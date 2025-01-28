open Nes

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

  let all = [Person; Dance; Book; Set; Tune; Version]

  module Set = struct
    include Stdlib.Set.Make(struct
        type nonrec t = t
        let compare = compare
      end)

    let all = of_list all
    let comp = diff all
  end

  let are_all l = Set.(equal (of_list l) all)

  let equal = (=)

  let to_string = function
    | Person -> "Person"
    | Dance -> "Dance"
    | Book -> "Book"
    | Set -> "Set"
    | Tune -> "Tune"
    | Version -> "Version"

  exception NotAType of string

  let of_string str =
    match String.lowercase_ascii str with
    | "person" -> Person
    | "dance" -> Dance
    | "book" -> Book
    | "set" -> Set
    | "tune" -> Tune
    | "version" -> Version
    | _ -> raise (NotAType str)

  let of_string_opt str =
    try
      Some (of_string str)
    with
    | NotAType _ -> None
end

let type_of = function
  | Person _ -> Type.Person
  | Dance _ -> Type.Dance
  | Book _ -> Type.Book
  | Set _ -> Type.Set
  | Tune _ -> Type.Tune
  | Version _ -> Type.Version
