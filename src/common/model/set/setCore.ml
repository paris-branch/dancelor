open Nes

let _key = "set"

type t =
  { slug : t Slug.t                  [@default Slug.none] ;
    status : Status.t                [@default Status.bot] ;
    name : string ;
    deviser : CreditCore.t Slug.t option [@default None] ;
    kind : Kind.dance ;
    versions_and_parameters : (VersionCore.t Slug.t * VersionParameters.t) list [@key "versions-and-parameters"] [@default []] ;
    order : SetOrder.t ;
    instructions : string            [@default ""] ;
    dances : DanceCore.t Slug.t list [@default []] ;
    remark : string                  [@default ""] }
[@@deriving make, yojson]

let make ?status ~slug ~name ?deviser ~kind ?versions_and_parameters ~order ?dances () =
  let%lwt deviser =
    let%optlwt deviser = Lwt.return deviser in
    let%lwt deviser = CreditCore.slug deviser in
    Lwt.return_some deviser
  in
  let%lwt versions_and_parameters =
    let%optlwt versions_and_parameters = Lwt.return versions_and_parameters in
    let%lwt versions_and_parameters =
      Lwt_list.map_s
        (fun (version, parameters) ->
           let%lwt slug = VersionCore.slug version in
           Lwt.return (slug, parameters))
        versions_and_parameters
    in
    Lwt.return_some versions_and_parameters
  in
  let%lwt dances =
    let%optlwt dances = Lwt.return dances in
    let%lwt dances = Lwt_list.map_p DanceCore.slug dances in
    Lwt.return_some dances
  in
  Lwt.return (make ?status ~slug ~name ~deviser ~kind ?versions_and_parameters ~order ?dances ())

let make_temp ~name ?deviser ~kind ?versions_and_parameters ~order ?dances () =
  make ~slug:Slug.none ~name ?deviser ~kind ?versions_and_parameters ~order ?dances ()

let slug s = Lwt.return s.slug
let is_slug_none s =
  let%lwt slug = slug s in
  Lwt.return (Slug.is_none slug)

let status s = Lwt.return s.status
let name s = Lwt.return s.name
let deviser s = Lwt.return s.deviser
let kind s = Lwt.return s.kind
let versions_and_parameters s = Lwt.return s.versions_and_parameters
let order s = Lwt.return s.order

let instructions s = Lwt.return s.instructions
let dances set = Lwt.return set.dances
let remark set = Lwt.return set.remark

let compare =
  compare_slugs_or
    ~fallback:(fun set1 set2 ->
        Lwt.return (Stdlib.compare set1 set2))
    slug

let equal = equal_from_compare compare

(* FIXME: use Version.equal *)
let contains_version slug1 set =
  List.exists
    (fun (slug2, _parameters) ->
       Slug.equal slug1 slug2)
    set.versions_and_parameters

type warning =
  | Empty
  | WrongKind
  | WrongVersionBars of VersionCore.t
  | WrongVersionKind of TuneCore.t
  | DuplicateVersion of TuneCore.t
[@@deriving yojson]

type warnings = warning list
[@@deriving yojson]
