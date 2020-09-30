module Self = struct
  type t =
    | Credit of Credit.t
    | Dance of Dance.t
    | Person of Person.t
    | Program of Program.t
    | Set of Set.t
    | Source of Source.t
    | Tune of Tune.t
    | Version of Version.t
  [@@deriving yojson]

  let _key = "any"
end
include Self

let constructor_to_string = function
  | Credit _ -> "Credit"
  | Dance _ -> "Dance"
  | Person _ -> "Person"
  | Program _ -> "Program"
  | Set _ -> "Set"
  | Source _ -> "Source"
  | Tune _ -> "Tune"
  | Version _ -> "Version"

module type S = sig
  type nonrec t = t

  val constructor_to_string : t -> string

  val search :
    (* ?filter:VersionFilter.t -> *)
    ?pagination:Pagination.t ->
    ?threshold:float -> string ->
    t Score.t list Lwt.t
end

module Arg = struct
  open Madge_common
  (* let filter = optarg (module VersionFilter) *)
  let pagination = optarg (module Pagination)
  let threshold = optarg ~key:"threshold" (module MFloat)
  let string = arg (module MString)
end

module Endpoint = struct
  open Madge_common
  let search = endpoint ~path:"/any/search" (module MList (Score.Make_Serialisable (Self)))
end
