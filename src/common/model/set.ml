open Nes

type t =
  { slug : t Slug.t ;
    name : string ;
    deviser : Credit.t Slug.t option [@default None] ;
    kind : Kind.dance ;
    status : Status.t                [@default Status.WorkInProgress] ;
    tunes : Tune.t Slug.t list       [@default []] }
[@@deriving make,yojson]

let slug s = Lwt.return s.slug
let name s = Lwt.return s.name
let deviser s = Lwt.return s.deviser
let kind s = Lwt.return s.kind
let status s = Lwt.return s.status
let tunes s = Lwt.return s.tunes

let contains t s = List.mem t s.tunes

let unsafe_make ~slug ~name ?deviser ~kind ?status ?tunes () =
  let%lwt deviser =
    match deviser with
    | None -> Lwt.return_none
    | Some deviser ->
      let%lwt deviser = Credit.slug deviser in
      Lwt.return_some deviser
  in
  let%lwt tunes =
    match tunes with
    | None -> Lwt.return_none
    | Some tunes ->
      let%lwt tunes = Lwt_list.map_s Tune.slug tunes in
      Lwt.return_some tunes
  in
  Lwt.return (make ~slug ~name ~deviser ~kind ?status ?tunes ())
