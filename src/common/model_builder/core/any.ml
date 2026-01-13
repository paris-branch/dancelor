open Nes

type t =
  | Person of Person.entry
  | Dance of Dance.entry
  | Source of Source.entry
  | Tune of Tune.entry
  | Version of Version.entry
  | Set of Set.entry
  | Book of Book.entry
  | User of User.entry
[@@deriving show {with_path = false}, yojson, variants]

(* NOTE: User is not added to [Any] on purpose. It is a bit of a special model
   that should not really be searchable or anything. *)

module Type = struct
  type t =
    | Person
    | Dance
    | Source
    | Tune
    | Version
    | Set
    | Book
    | User
  [@@deriving eq, show {with_path = false}, yojson]

  let compare t1 t2 =
    let to_int = function
      | Person -> 0
      | Dance -> 1
      | Source -> 2
      | Tune -> 3
      | Version -> 4
      | Set -> 5
      | Book -> 6
      | User -> 7
    in
    if t1 = t2 then
      0
    else
      Int.compare (to_int t1) (to_int t2)

  let all = [Person; Dance; Source; Tune; Version; Set; Book; User]

  module Set = struct
    include Stdlib.Set.Make(struct
      type nonrec t = t
      let compare = compare
    end)

    let all = of_list all
    let comp = diff all
  end

  let equal = (=)

  let to_string = function
    | Person -> "Person"
    | Dance -> "Dance"
    | Source -> "Source"
    | Tune -> "Tune"
    | Version -> "Version"
    | Set -> "Set"
    | Book -> "Book"
    | User -> "User"

  exception Not_a_type of string

  let of_string str =
    match String.lowercase_ascii str with
    | "person" -> Person
    | "dance" -> Dance
    | "source" -> Source
    | "tune" -> Tune
    | "version" -> Version
    | "set" -> Set
    | "book" -> Book
    | "user" -> User
    | _ -> raise (Not_a_type str)

  let of_string_opt str =
    try
      Some (of_string str)
    with
      | Not_a_type _ -> None
end

let type_of = function
  | Person _ -> Type.Person
  | Dance _ -> Type.Dance
  | Source _ -> Type.Source
  | Tune _ -> Type.Tune
  | Version _ -> Type.Version
  | Set _ -> Type.Set
  | Book _ -> Type.Book
  | User _ -> Type.User

let to_entry = function
  | Person entry -> Entry.unsafe_erase_value_and_access entry
  | Dance entry -> Entry.unsafe_erase_value_and_access entry
  | Source entry -> Entry.unsafe_erase_value_and_access entry
  | Tune entry -> Entry.unsafe_erase_value_and_access entry
  | Version entry -> Entry.unsafe_erase_value_and_access entry
  | Set entry -> Entry.unsafe_erase_value_and_access entry
  | Book entry -> Entry.unsafe_erase_value_and_access entry
  | User entry -> Entry.unsafe_erase_value_and_access entry

let to_entry' ~on_public ~on_private = function
  | Person entry -> on_public (Entry.unsafe_erase_value entry)
  | Dance entry -> on_public (Entry.unsafe_erase_value entry)
  | Source entry -> on_public (Entry.unsafe_erase_value entry)
  | Tune entry -> on_public (Entry.unsafe_erase_value entry)
  | Version entry -> on_public (Entry.unsafe_erase_value entry)
  | Set entry -> on_private (Entry.unsafe_erase_value entry)
  | Book entry -> on_private (Entry.unsafe_erase_value entry)
  | User entry -> on_public (Entry.unsafe_erase_value entry)
