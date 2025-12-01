open Nes
open Madge
open ModelBuilder.Core
module Filter = FilterBuilder.Core

type copyright_response_reason =
  | Connected
  | Composer_agrees
  | Publisher_agrees of Source.t Entry.t
[@@deriving yojson]

type 'payload copyright_response =
  | Protected
  | Granted of {payload: 'payload; reason: copyright_response_reason}
[@@deriving yojson]

type (_, _, _) t =
(* Actions without specific version *)
| Create : ((Version.t -> 'w), 'w, Version.t Entry.t) t
| Search : ((Slice.t -> Filter.Version.t -> 'w), 'w, (int * Version.t Entry.t list)) t
(* Actions on a specific version *)
| Get : ((Version.t Entry.Id.t -> 'w), 'w, Version.t Entry.t) t
| Content : ((Version.t Entry.Id.t -> 'w), 'w, Version.Content.t copyright_response) t
| Update : ((Version.t Entry.Id.t -> Version.t -> 'w), 'w, Version.t Entry.t) t
| Delete : ((Version.t Entry.Id.t -> 'w), 'w, unit) t
(* Files related to a version *)
| BuildSvg : ((Version.t Entry.Id.t -> VersionParameters.t -> RenderingParameters.t -> 'w), 'w, JobId.t Job.registration_response copyright_response) t
| BuildOgg : ((Version.t Entry.Id.t -> VersionParameters.t -> RenderingParameters.t -> 'w), 'w, JobId.t Job.registration_response copyright_response) t
| BuildPdf : ((Version.t Entry.Id.t -> VersionParameters.t -> RenderingParameters.t -> 'w), 'w, JobId.t Job.registration_response copyright_response) t
(* Files related to an anonymous version *)
| BuildSvg' : ((Version.t -> VersionParameters.t -> RenderingParameters.t -> 'w), 'w, JobId.t Job.registration_response) t
| BuildOgg' : ((Version.t -> VersionParameters.t -> RenderingParameters.t -> 'w), 'w, JobId.t Job.registration_response) t
[@@deriving madge_wrapped_endpoints]

(* NOTE: The version model contains its LilyPond content. This is a big string
   that is not used in the client. It would be better to have a clean way to
   describe fields that are not included by default, but for now we will just
   redact it from the HTTP responses. NOTE: We only redact it from the HTTP
   _responses_, but not from the requests! *)
module VersionNoLilypond = struct
  type t = Version.t
  let of_yojson = Version.of_yojson
  let to_yojson = Version.to_yojson % Version.erase_lilypond_from_content
end

module Copyright_response
    (Payload : Madge.JSONABLE)
  : Madge.JSONABLE with type t = Payload.t copyright_response
= struct
  type t = Payload.t copyright_response [@@deriving yojson]
end

let route : type a w r. (a, w, r) t -> (a, w, r) route =
  let open Route in
  function
    (* Actions without specific version *)
    | Create -> body "version" (module Version) @@ post (module Entry.J(VersionNoLilypond))
    | Search -> query "slice" (module Slice) @@ query "filter" (module Filter.Version) @@ get (module JPair(JInt)(JList(Entry.J(VersionNoLilypond))))
    (* Actions on a specific version *)
    | Get -> variable (module Entry.Id.S(Version)) @@ get (module Entry.J(VersionNoLilypond))
    | Content -> literal "content" @@ variable (module Entry.Id.S(Version)) @@ get (module Copyright_response(Version.Content))
    | Update -> variable (module Entry.Id.S(Version)) @@ body "version" (module Version) @@ put (module Entry.J(VersionNoLilypond))
    | Delete -> variable (module Entry.Id.S(Version)) @@ delete (module JUnit)
    (* Files related to a version *)
    | BuildSvg -> literal "build-svg" @@ variable (module Entry.Id.S(Version)) @@ query "parameters" (module VersionParameters) @@ query "rendering-parameters" (module RenderingParameters) @@ post (module Copyright_response(Job.Registration_response(JobId)))
    | BuildOgg -> literal "build-ogg" @@ variable (module Entry.Id.S(Version)) @@ query "parameters" (module VersionParameters) @@ query "rendering-parameters" (module RenderingParameters) @@ post (module Copyright_response(Job.Registration_response(JobId)))
    | BuildPdf -> literal "build-pdf" @@ variable (module Entry.Id.S(Version)) @@ query "parameters" (module VersionParameters) @@ query "rendering-parameters" (module RenderingParameters) @@ post (module Copyright_response(Job.Registration_response(JobId)))
    (* Files related to an anonymous version *)
    | BuildSvg' -> literal "build-svg" @@ query "version" (module Version) @@ query "parameters" (module VersionParameters) @@ query "rendering-parameters" (module RenderingParameters) @@ post (module Job.Registration_response(JobId))
    | BuildOgg' -> literal "build-ogg" @@ query "version" (module Version) @@ query "parameters" (module VersionParameters) @@ query "rendering-parameters" (module RenderingParameters) @@ post (module Job.Registration_response(JobId))
