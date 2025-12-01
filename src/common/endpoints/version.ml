open Nes
open Madge
open ModelBuilder.Core
module Filter = FilterBuilder.Core

module Copyright_response = struct
  type reason =
    | Connected
    | Composer_agrees
    | Publisher_agrees of Source.t Entry.t
  [@@deriving yojson]

  type 'payload response =
    | Protected
    | Granted of {payload: 'payload; reason: reason}
  [@@deriving yojson]

  module Content = struct
    type t = Version.Content.t response
    [@@deriving yojson]
  end

  module Job_registration = struct
    type t = Job.Registration.t response
    [@@deriving yojson]
  end
end

type (_, _, _) t =
(* Actions without specific version *)
| Create : ((Version.t -> 'w), 'w, Version.t Entry.t) t
| Search : ((Slice.t -> Filter.Version.t -> 'w), 'w, (int * Version.t Entry.t list)) t
(* Actions on a specific version *)
| Get : ((Version.t Entry.Id.t -> 'w), 'w, Version.t Entry.t) t
| Content : ((Version.t Entry.Id.t -> 'w), 'w, Copyright_response.Content.t) t
| Update : ((Version.t Entry.Id.t -> Version.t -> 'w), 'w, Version.t Entry.t) t
| Delete : ((Version.t Entry.Id.t -> 'w), 'w, unit) t
(* Files related to a version *)
| BuildSvg : ((Version.t Entry.Id.t -> VersionParameters.t -> RenderingParameters.t -> 'w), 'w, Copyright_response.Job_registration.t) t
| BuildOgg : ((Version.t Entry.Id.t -> VersionParameters.t -> RenderingParameters.t -> 'w), 'w, Copyright_response.Job_registration.t) t
| BuildPdf : ((Version.t Entry.Id.t -> VersionParameters.t -> RenderingParameters.t -> 'w), 'w, Copyright_response.Job_registration.t) t
(* Files related to an anonymous version *)
| BuildSvg' : ((Version.t -> VersionParameters.t -> RenderingParameters.t -> 'w), 'w, Job.Registration.t) t
| BuildOgg' : ((Version.t -> VersionParameters.t -> RenderingParameters.t -> 'w), 'w, Job.Registration.t) t
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

let route : type a w r. (a, w, r) t -> (a, w, r) route =
  let open Route in
  function
    (* Actions without specific version *)
    | Create -> body "version" (module Version) @@ post (module Entry.J(VersionNoLilypond))
    | Search -> query "slice" (module Slice) @@ query "filter" (module Filter.Version) @@ get (module JPair(JInt)(JList(Entry.J(VersionNoLilypond))))
    (* Actions on a specific version *)
    | Get -> variable (module Entry.Id.S(Version)) @@ get (module Entry.J(VersionNoLilypond))
    | Content -> literal "content" @@ variable (module Entry.Id.S(Version)) @@ get (module Copyright_response.Content)
    | Update -> variable (module Entry.Id.S(Version)) @@ body "version" (module Version) @@ put (module Entry.J(VersionNoLilypond))
    | Delete -> variable (module Entry.Id.S(Version)) @@ delete (module JUnit)
    (* Files related to a version *)
    | BuildSvg -> literal "build-svg" @@ variable (module Entry.Id.S(Version)) @@ query "parameters" (module VersionParameters) @@ query "rendering-parameters" (module RenderingParameters) @@ post (module Copyright_response.Job_registration)
    | BuildOgg -> literal "build-ogg" @@ variable (module Entry.Id.S(Version)) @@ query "parameters" (module VersionParameters) @@ query "rendering-parameters" (module RenderingParameters) @@ post (module Copyright_response.Job_registration)
    | BuildPdf -> literal "build-pdf" @@ variable (module Entry.Id.S(Version)) @@ query "parameters" (module VersionParameters) @@ query "rendering-parameters" (module RenderingParameters) @@ post (module Copyright_response.Job_registration)
    (* Files related to an anonymous version *)
    | BuildSvg' -> literal "build-svg" @@ query "version" (module Version) @@ query "parameters" (module VersionParameters) @@ query "rendering-parameters" (module RenderingParameters) @@ post (module Job.Registration)
    | BuildOgg' -> literal "build-ogg" @@ query "version" (module Version) @@ query "parameters" (module VersionParameters) @@ query "rendering-parameters" (module RenderingParameters) @@ post (module Job.Registration)
