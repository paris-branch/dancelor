open Nes

let _key = "tune"

type t =
  { slug : t Slug.t ;
    status : Status.t                   [@default Status.bot] ;
    name : string ;
    alternative_names : string list     [@key "alternative-names"] [@default []] ;
    kind : Kind.base ;
    author : CreditCore.t Slug.t option [@default None] ;
    dances : DanceCore.t Slug.t list    [@default []] ;
    remark : string                     [@default ""] ;
    scddb_id : int option               [@default None] [@key "scddb-id"] ;
    modified_at : Datetime.t            [@key "modified-at"] ;
    created_at  : Datetime.t            [@key "created-at"] }
[@@deriving make, yojson]

let make ?status ~slug ~name ?alternative_names ~kind ?author ?dances
    ?remark ?scddb_id ~modified_at () =
  let%lwt author =
    let%olwt author = Lwt.return author in
    let%lwt author_slug = CreditCore.slug author in
    Lwt.return_some author_slug
  in
  let%lwt dances =
    let%olwt dances = Lwt.return dances in
    let%lwt dances =
      Lwt_list.map_s
        (fun dance ->
           let%lwt dance = DanceCore.slug dance in
           Lwt.return dance)
        dances
    in
    Lwt.return_some dances
  in
  Lwt.return (make ?status ~slug ~name ?alternative_names ~kind ~author ?dances
                ?remark ~scddb_id ~modified_at ())

let slug tune = Lwt.return tune.slug
let status tune = Lwt.return tune.status
let name tune = Lwt.return tune.name
let alternative_names tune = Lwt.return tune.alternative_names
let kind tune = Lwt.return tune.kind
let author tune = Lwt.return tune.author
let dances tune = Lwt.return tune.dances
let remark tune = Lwt.return tune.remark
let scddb_id tune = Lwt.return tune.scddb_id

let compare =
  Slug.compare_slugs_or
    ~fallback:(fun tune1 tune2 ->
        Lwt.return (Stdlib.compare tune1 tune2))
    slug

let equal = equal_from_compare compare
