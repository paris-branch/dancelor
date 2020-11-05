open Nes

module Self = struct
  type t =
    { slug : t Slug.t ;
      status : Status.t [@default Status.bot] ;
      name : string ;
      date : Date.t ;
      sets_and_parameters : (Set.t Slug.t * SetParameters.t) list [@key "sets-and-parameters"] ;
      remark : string [@default ""] }
  [@@deriving yojson]

  let _key = "program"
end
include Self

let slug p = Lwt.return p.slug
let status p = Lwt.return p.status
let name p = Lwt.return p.name
let date p = Lwt.return p.date
let sets_and_parameters p = Lwt.return p.sets_and_parameters
let remark program = Lwt.return program.remark

let contains_set slug1 program =
  List.exists
    (fun (slug2, _parameters) ->
       Slug.equal slug1 slug2)
    program.sets_and_parameters

let compare p1 p2 =
  (* Compare first by date *)
  let c = compare p1.date p2.date in
  if c = 0 then
    compare p1 p2
  else
    c

type warning =
  | Empty
  | DuplicateSet of Set.t (* FIXME: duplicate dance? *)
  | DuplicateVersion of Tune.t
[@@deriving yojson]

type warnings = warning list
[@@deriving yojson]

module type S = sig
  type nonrec t = t

  val slug : t -> t Slug.t Lwt.t
  val status : t -> Status.t Lwt.t
  val name : t -> string Lwt.t
  val date : t -> Date.t Lwt.t
  val sets_and_parameters : t -> (Set.t * SetParameters.t) list Lwt.t
  val remark : t -> string Lwt.t

  val contains_set : Set.t Slug.t -> t -> bool
  val compare : t -> t -> int

  (* {2 Warnings} *)

  type warning =
    | Empty
    | DuplicateSet of Set.t (* FIXME: duplicate dance? *)
    | DuplicateVersion of Tune.t

  type warnings = warning list

  val warnings : t -> warnings Lwt.t

  (** {2 Getters and setters} *)

  val get : t Slug.t -> t Lwt.t

  val get_all : unit -> t list Lwt.t

  val search :
    ?pagination:Pagination.t ->
    ?threshold:float ->
    string ->
    t Score.t list Lwt.t
end

module Arg = struct
  open Madge_common
  let slug = arg ~key:"slug" (module MString)
  let status = optarg (module Status)
  let pagination = optarg (module Pagination)
  let threshold = optarg ~key:"threshold" (module MFloat)
  let string = arg (module MString)
end

module Endpoint = struct
  open Madge_common
  let get = endpoint ~path:"/program" (module Self)
  let get_all = endpoint ~path:"/program/all" (module MList (Self))
  let search = endpoint ~path:"/program/search" (module MList (Score.Make_Serialisable (Self)))
end
