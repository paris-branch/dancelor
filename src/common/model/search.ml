open Nes

module Results = struct
  type t =
    { persons :     float * Person.t    Slug.t Score.t list ;
      credits :     float * Credit.t    Slug.t Score.t list ;
      tunes :       float * Tune.t      Slug.t Score.t list ;
      tune_groups : float * TuneGroup.t Slug.t Score.t list ;
      programs :    float * Program.t   Slug.t Score.t list ;
      sets :        float * Set.t       Slug.t Score.t list ;
      dances :      float * Dance.t     Slug.t Score.t list ;
      sources :     float * Source.t    Slug.t Score.t list }
  [@@deriving yojson]

  let _key = "results"
end

module type S = sig
  val search :
    ?pagination:Pagination.t ->
    ?threshold:float ->
    string ->
    Results.t Lwt.t
end

module Arg = struct
  open Madge_common
  let pagination = optarg (module Pagination)
  let threshold = optarg ~key:"threshold" (module MFloat)
  let string = arg (module MString)
end

module Endpoint = struct
  open Madge_common
  let search = endpoint ~path:"/search" (module Results)
end
