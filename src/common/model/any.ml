module Self = struct
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
end
include Self

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

module type S = sig
  type nonrec t = t

  module Type = Type

  val type_of : t -> Type.t

  val search :
    (* ?filter:VersionFilter.t -> *)
    ?pagination:Pagination.t ->
    ?threshold:float ->
    ?except:Type.t list ->
    string ->
    t Score.t list Lwt.t
end

module Arg = struct
  open Madge_common
  (* let filter = optarg (module VersionFilter) *)
  let pagination = optarg (module Pagination)
  let threshold = optarg ~key:"threshold" (module MFloat)
  let type_ = optarg ~key:"type" (module MList (Type))
  let string = arg (module MString)
end

module Endpoint = struct
  open Madge_common
  let search = endpoint ~path:"/any/search" (module MList (Score.Make_Serialisable (Self)))
end
