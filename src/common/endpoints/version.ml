open Nes
open Madge
open Model_builder.Core
module Filter = Filter_builder.Core

type copyright_response_reason =
  | Connected
  | Composer_agrees
  | Publisher_agrees of Source.entry
[@@deriving yojson]

type 'payload copyright_response =
  | Protected
  | Granted of {payload: 'payload; reason: copyright_response_reason}
[@@deriving yojson]

let map_copyright_response f = function
  | Protected -> Protected
  | Granted {payload; reason} -> Granted {payload = f payload; reason}

module Snippet_ids = struct
  type t = {
    svg_job_id: Job_id.t;
    ogg_job_id: Job_id.t;
  }
  [@@deriving yojson]
end

type (_, _, _) t =
(* Actions without specific version *)
| Create : ((Version.t -> 'w), 'w, Version.entry) t
| Search : ((Slice.t -> Filter.Version.t -> 'w), 'w, (int * Version.entry list)) t
(* Actions on a specific version *)
| Get : ((Version.t Entry.Id.t -> 'w), 'w, Version.entry) t
| Content : ((Version.t Entry.Id.t -> 'w), 'w, Version.Content.t copyright_response) t
| Update : ((Version.t Entry.Id.t -> Version.t -> 'w), 'w, Version.entry) t
| Delete : ((Version.t Entry.Id.t -> 'w), 'w, unit) t
(* Files related to a version *)
| Build_snippets : ((Version.t Entry.Id.t -> Version_parameters.t -> Rendering_parameters.t -> 'w), 'w, Snippet_ids.t Job.registration_response copyright_response) t
| Build_pdf : ((Version.t Entry.Id.t -> Version_parameters.t -> Rendering_parameters.t -> 'w), 'w, Job_id.t Job.registration_response copyright_response) t
(* Files related to an anonymous version *)
| Build_snippets' : ((Version.t -> Version_parameters.t -> Rendering_parameters.t -> 'w), 'w, Snippet_ids.t Job.registration_response) t
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
    | Create -> body "version" (module Version) @@ post (module Entry.JPublic(VersionNoLilypond))
    | Search -> query "slice" (module Slice) @@ query "filter" (module Filter.Version) @@ get (module JPair(JInt)(JList(Entry.JPublic(VersionNoLilypond))))
    (* Actions on a specific version *)
    | Get -> variable (module Entry.Id.S(Version)) @@ get (module Entry.JPublic(VersionNoLilypond))
    | Content -> literal "content" @@ variable (module Entry.Id.S(Version)) @@ get (module Copyright_response(Version.Content))
    | Update -> variable (module Entry.Id.S(Version)) @@ body "version" (module Version) @@ put (module Entry.JPublic(VersionNoLilypond))
    | Delete -> variable (module Entry.Id.S(Version)) @@ delete (module JUnit)
    (* Files related to a version *)
    | Build_snippets -> literal "build-snippets" @@ variable (module Entry.Id.S(Version)) @@ query "parameters" (module Version_parameters) @@ query "rendering-parameters" (module Rendering_parameters) @@ post (module Copyright_response(Job.Registration_response(Snippet_ids)))
    | Build_pdf -> literal "build-pdf" @@ variable (module Entry.Id.S(Version)) @@ query "parameters" (module Version_parameters) @@ query "rendering-parameters" (module Rendering_parameters) @@ post (module Copyright_response(Job.Registration_response(Job_id)))
    (* Files related to an anonymous version *)
    | Build_snippets' -> literal "build-snippets" @@ query "version" (module Version) @@ query "parameters" (module Version_parameters) @@ query "rendering-parameters" (module Rendering_parameters) @@ post (module Job.Registration_response(Snippet_ids))
