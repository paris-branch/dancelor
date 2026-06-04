open Nes
open Madge
open Model_new
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

module Version_view_fallback = struct
  type t =
    | Found of Version_view.t
    | Fallback of Tune_view.t
  [@@deriving yojson]
end

type (_, _, _) t =
  | Create : (Version.t -> 'w, 'w, Version_id.t) t
  | Search : (Slice.t -> (Version.t, Filter.Version.t) Formula_entry.public -> 'w, 'w, Version_row.t search_result) t
  | Get : (Version_id.t -> 'w, 'w, Version.entry) t
  | Get_row : (Version_id.t -> 'w, 'w, Version_row.t) t
  | Get_view : (Version_id.t -> 'w, 'w, Version_view.t) t
  | Get_view_for_tune : (Tune_id.t -> 'w, 'w, Version_view_fallback.t) t (** looks for a version for the given tune and return it, or falls back on the tune *)
  | Content : (Version_id.t -> 'w, 'w, Version.Content.t copyright_response) t
  | Update : (Version_id.t -> Version.t -> 'w, 'w, unit) t
  | Delete : (Version_id.t -> 'w, 'w, unit) t
  | Build_snippets : (Version_id.t -> Version_parameters.t -> Rendering_parameters.t -> 'w, 'w, Snippet_ids.t Job.registration_response copyright_response) t
  | Build_pdf : (Version_id.t -> Version_parameters.t -> Rendering_parameters.t -> 'w, 'w, Job_id.t Job.registration_response copyright_response) t
  | Build_snippets' : (Version.t -> Version_parameters.t -> Rendering_parameters.t -> 'w, 'w, Snippet_ids.t Job.registration_response) t
[@@deriving madge_wrapped_endpoints]

(* NOTE: The version model contains its LilyPond content. This is a big string
   that is not used in the client. It would be better to have a clean way to
   describe fields that are not included by default, but for now we will just
   redact it from the HTTP responses. NOTE: We only redact it from the HTTP
   _responses_, but not from the requests! *)
module Version_no_lilypond = struct
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
    | Create -> body "version" (module Version) @@ post (module Version_id)
    | Search -> query "slice" (module Slice) @@ query "filter" (module Formula_entry.JPublic(Version)(Filter.Version)) @@ get (module Utils.Search_result(Version_row))
    | Get -> variable (module Version_id) @@ get (module Entry.JPublic(Version_no_lilypond))
    | Get_row -> variable (module Version_id) @@ literal "row" @@ get (module Version_row)
    | Get_view -> variable (module Version_id) @@ literal "view" @@ get (module Version_view)
    | Get_view_for_tune -> literal "for-tune" @@ variable (module Tune_id) @@ literal "view" @@ get (module Version_view_fallback)
    | Content -> literal "content" @@ variable (module Version_id) @@ get (module Copyright_response(Version.Content))
    | Update -> variable (module Version_id) @@ body "version" (module Version) @@ put (module JUnit)
    | Delete -> variable (module Version_id) @@ delete (module JUnit)
    | Build_snippets -> literal "build-snippets" @@ variable (module Version_id) @@ query "parameters" (module Version_parameters) @@ query "rendering-parameters" (module Rendering_parameters) @@ post (module Copyright_response(Job.Registration_response(Snippet_ids)))
    | Build_pdf -> literal "build-pdf" @@ variable (module Version_id) @@ query "parameters" (module Version_parameters) @@ query "rendering-parameters" (module Rendering_parameters) @@ post (module Copyright_response(Job.Registration_response(Job_id)))
    (* Files related to an anonymous version *)
    | Build_snippets' -> literal "build-snippets" @@ query "version" (module Version) @@ query "parameters" (module Version_parameters) @@ query "rendering-parameters" (module Rendering_parameters) @@ post (module Job.Registration_response(Snippet_ids))
