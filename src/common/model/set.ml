open Nes

type t =
  { slug : t Slug.t                  [@default Slug.none] ;
    status : Status.t                [@default Status.bot] ;
    name : string ;
    deviser : Credit.t Slug.t option [@default None] ;
    kind : Kind.dance ;
    versions_and_parameters : (Version.t Slug.t * VersionParameters.t) list [@key "versions-and-parameters"] [@default []] ;
    instructions : string            [@default ""] ;
    dances : Dance.t Slug.t list     [@default []] ;
    remark : string                  [@default ""] ;
    sources : Source.t Slug.t list   [@default []] }
[@@deriving make, yojson]

let _key = "set"

let make ?status ~slug ~name ?deviser ~kind ?versions_and_parameters ?dances () =
  let%lwt deviser =
    match deviser with
    | None -> Lwt.return_none
    | Some deviser ->
      let%lwt deviser = Credit.slug deviser in
      Lwt.return_some deviser
  in
  let%lwt versions_and_parameters =
    match versions_and_parameters with
    | None -> Lwt.return_none
    | Some versions_and_parameters ->
      let%lwt versions_and_parameters =
        Lwt_list.map_s
          (fun (version, parameters) ->
             let%lwt slug = Version.slug version in
             Lwt.return (slug, parameters))
          versions_and_parameters
      in
      Lwt.return_some versions_and_parameters
  in
  let%lwt dances =
    match dances with
    | None -> Lwt.return_none
    | Some dances ->
      let%lwt dances = Lwt_list.map_p Dance.slug dances in
      Lwt.return_some dances
  in
  Lwt.return (make ?status ~slug ~name ~deviser ~kind ?versions_and_parameters ?dances ())

let make_temp ~name ?deviser ~kind ?versions_and_parameters ?dances () =
  make ~slug:Slug.none ~name ?deviser ~kind ?versions_and_parameters ?dances ()

let slug s = Lwt.return s.slug
let status s = Lwt.return s.status
let name s = Lwt.return s.name
let deviser s = Lwt.return s.deviser
let kind s = Lwt.return s.kind
let versions_and_parameters s = Lwt.return s.versions_and_parameters
let instructions s = Lwt.return s.instructions
let dances set = Lwt.return set.dances
let remark set = Lwt.return set.remark

let contains_version slug1 set =
  List.exists
    (fun (slug2, _parameters) ->
       Slug.equal slug1 slug2)
    set.versions_and_parameters

type warning =
  | Empty
  | WrongKind
  | WrongVersionBars of Version.t
  | WrongVersionKind of Tune.t
  | DuplicateVersion of Tune.t
[@@deriving yojson]

type warnings = warning list
[@@deriving yojson]
