open Nes
open Dancelor_common_database

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
