open Nes

module CommonResults = struct
  type t =
    { persons :     float * Person.t    Slug.t Score.t list ;
      credits :     float * Credit.t    Slug.t Score.t list ;
      versions :       float * Version.t      Slug.t Score.t list ;
      tunes : float * Tune.t Slug.t Score.t list ;
      programs :    float * Program.t   Slug.t Score.t list ;
      sets :        float * Set.t       Slug.t Score.t list ;
      dances :      float * Dance.t     Slug.t Score.t list ;
      sources :     float * Source.t    Slug.t Score.t list }
  [@@deriving yojson]

  let persons r = Lwt.return r.persons
  let credits r = Lwt.return r.credits
  let versions r = Lwt.return r.versions
  let tunes r = Lwt.return r.tunes
  let programs r = Lwt.return r.programs
  let sets r = Lwt.return r.sets
  let dances r = Lwt.return r.dances
  let sources r = Lwt.return r.sources

  let _key = "results"
end

module type S = sig
  module Results : sig
    type t

    val persons : t -> (float * Person.t Score.t list) Lwt.t
    val credits : t -> (float * Credit.t Score.t list) Lwt.t
    val versions : t -> (float * Version.t Score.t list) Lwt.t
    val tunes : t -> (float * Tune.t Score.t list) Lwt.t
    val programs : t -> (float * Program.t Score.t list) Lwt.t
    val sets : t -> (float * Set.t Score.t list) Lwt.t
    val dances : t -> (float * Dance.t Score.t list) Lwt.t
    val sources : t -> (float * Source.t Score.t list) Lwt.t
  end

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
  let search = endpoint ~path:"/search" (module CommonResults)
end
