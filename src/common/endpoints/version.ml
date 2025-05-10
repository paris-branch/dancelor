open Nes
open Madge
open ModelBuilder

type (_, _, _) t =
(* Actions without specific version *)
| Create : ((Core.Version.t -> 'w), 'w, Core.Version.t Entry.t) t
| Search : ((Slice.t -> Filter.Version.t -> 'w), 'w, (int * Core.Version.t Entry.t list)) t
(* Actions on a specific version *)
| Get : ((Core.Version.t Slug.t -> 'w), 'w, Core.Version.t Entry.t) t
| Update : ((Core.Version.t Slug.t -> Core.Version.t -> 'w), 'w, Core.Version.t Entry.t) t
(* Files related to a version *)
| Ly : ((Core.Version.t Slug.t -> 'w), 'w, Void.t) t
| Svg : ((Core.Version.t Slug.t -> Core.VersionParameters.t -> RenderingParameters.t -> 'w), 'w, Void.t) t
| Ogg : ((Core.Version.t Slug.t -> Core.VersionParameters.t -> RenderingParameters.t -> 'w), 'w, Void.t) t
| Pdf : ((Core.Version.t Slug.t -> Core.VersionParameters.t -> RenderingParameters.t -> 'w), 'w, Void.t) t
(* Files related to an anonymous version *)
| PreviewSvg : ((Core.Version.t -> Core.VersionParameters.t -> RenderingParameters.t -> 'w), 'w, Void.t) t
| PreviewOgg : ((Core.Version.t -> Core.VersionParameters.t -> RenderingParameters.t -> 'w), 'w, Void.t) t
[@@deriving madge_wrapped_endpoints]

(* NOTE: The version model contains its LilyPond content. This is a big string
   that is not used in the client. It would be better to have a clean way to
   describe fields that are not included by default, but for now we will just
   redact it from the HTTP responses. NOTE: We only redact it from the HTTP
   _responses_, but not from the requests! *)
module VersionNoContent = struct
  type t = Core.Version.t
  let of_yojson = Core.Version.of_yojson % Json.add_field "content" (`String "")
  let to_yojson = Json.remove_field "content" % Core.Version.to_yojson
end

let route : type a w r. (a, w, r) t -> (a, w, r) route =
  let open Route in
  function
    (* Actions without specific version *)
    | Create -> body "version" (module Core.Version) @@ post (module Entry.J(VersionNoContent))
    | Search -> query "slice" (module Slice) @@ query "filter" (module Filter.Version) @@ get (module JPair(JInt)(JList(Entry.J(VersionNoContent))))
    (* Actions on a specific version *)
    | Get -> variable (module SSlug(Core.Version)) @@ get (module Entry.J(VersionNoContent))
    | Update -> variable (module SSlug(Core.Version)) @@ body "version" (module Core.Version) @@ put (module Entry.J(VersionNoContent))
    (* Files related to a version *)
    | Ly -> variable (module SSlug(Core.Version)) ~suffix: ".ly" @@ void ()
    | Svg -> variable (module SSlug(Core.Version)) ~suffix: ".svg" @@ query "parameters" (module Core.VersionParameters) @@ query "rendering-parameters" (module RenderingParameters) @@ void ()
    | Ogg -> variable (module SSlug(Core.Version)) ~suffix: ".ogg" @@ query "parameters" (module Core.VersionParameters) @@ query "rendering-parameters" (module RenderingParameters) @@ void ()
    | Pdf -> variable (module SSlug(Core.Version)) ~suffix: ".pdf" @@ query "parameters" (module Core.VersionParameters) @@ query "rendering-parameters" (module RenderingParameters) @@ void ()
    (* Files related to an anonymous version *)
    | PreviewSvg -> query "version" (module Core.Version) @@ literal "preview.svg" @@ query "parameters" (module Core.VersionParameters) @@ query "rendering-parameters" (module RenderingParameters) @@ void ()
    | PreviewOgg -> query "version" (module Core.Version) @@ literal "preview.ogg" @@ query "parameters" (module Core.VersionParameters) @@ query "rendering-parameters" (module RenderingParameters) @@ void ()
