open Nes
open Madge
open ModelBuilder

type (_, _, _) t =
(* Actions without specific version *)
| Create : ((Version.t -> 'w), 'w, Version.t Entry.t) t
| Search : ((Slice.t -> Version.Filter.t -> 'w), 'w, (int * Version.t Entry.t list)) t
(* Actions on a specific version *)
| Get : ((Version.t Slug.t -> 'w), 'w, Version.t Entry.t) t
| Update : ((Version.t Slug.t -> Version.t -> 'w), 'w, Version.t Entry.t) t
(* Files related to a version *)
| Ly : ((Version.t Slug.t -> 'w), 'w, Void.t) t
| Svg : ((Version.t Slug.t -> VersionParameters.t -> RenderingParameters.t -> 'w), 'w, Void.t) t
| Ogg : ((Version.t Slug.t -> VersionParameters.t -> RenderingParameters.t -> 'w), 'w, Void.t) t
| Pdf : ((Version.t Slug.t -> VersionParameters.t -> RenderingParameters.t -> 'w), 'w, Void.t) t
(* Files related to an anonymous version *)
| PreviewSvg : ((Version.t -> VersionParameters.t -> RenderingParameters.t -> 'w), 'w, Void.t) t
| PreviewOgg : ((Version.t -> VersionParameters.t -> RenderingParameters.t -> 'w), 'w, Void.t) t
[@@deriving madge_wrapped_endpoints]

(* NOTE: The version model contains its LilyPond content. This is a big string
   that is not used in the client. It would be better to have a clean way to
   describe fields that are not included by default, but for now we will just
   redact it from the HTTP responses. NOTE: We only redact it from the HTTP
   _responses_, but not from the requests! *)
module VersionNoContent = struct
  type t = Version.t
  let of_yojson = Version.of_yojson % Json.add_field "content" (`String "")
  let to_yojson = Json.remove_field "content" % Version.to_yojson
end

let route : type a w r. (a, w, r) t -> (a, w, r) route =
  let open Route in
  function
    (* Actions without specific version *)
    | Create -> body "version" (module Version) @@ post (module Entry.J(VersionNoContent))
    | Search -> query "slice" (module Slice) @@ query "filter" (module Version.Filter) @@ get (module JPair(JInt)(JList(Entry.J(VersionNoContent))))
    (* Actions on a specific version *)
    | Get -> variable (module SSlug(Version)) @@ get (module Entry.J(VersionNoContent))
    | Update -> variable (module SSlug(Version)) @@ body "version" (module Version) @@ put (module Entry.J(VersionNoContent))
    (* Files related to a version *)
    | Ly -> variable (module SSlug(Version)) ~suffix: ".ly" @@ void ()
    | Svg -> variable (module SSlug(Version)) ~suffix: ".svg" @@ query "parameters" (module VersionParameters) @@ query "rendering-parameters" (module RenderingParameters) @@ void ()
    | Ogg -> variable (module SSlug(Version)) ~suffix: ".ogg" @@ query "parameters" (module VersionParameters) @@ query "rendering-parameters" (module RenderingParameters) @@ void ()
    | Pdf -> variable (module SSlug(Version)) ~suffix: ".pdf" @@ query "parameters" (module VersionParameters) @@ query "rendering-parameters" (module RenderingParameters) @@ void ()
    (* Files related to an anonymous version *)
    | PreviewSvg -> query "version" (module Version) @@ literal "preview.svg" @@ query "parameters" (module VersionParameters) @@ query "rendering-parameters" (module RenderingParameters) @@ void ()
    | PreviewOgg -> query "version" (module Version) @@ literal "preview.ogg" @@ query "parameters" (module VersionParameters) @@ query "rendering-parameters" (module RenderingParameters) @@ void ()
