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
| Svg : ((VersionParameters.t -> Version.t Slug.t -> 'w), 'w, Void.t) t
| Ogg : ((VersionParameters.t -> Version.t Slug.t -> 'w), 'w, Void.t) t
| Pdf : ((VersionParameters.t -> Version.t Slug.t -> 'w), 'w, Void.t) t
(* Files related to an anonymous version *)
| PreviewSvg : ((VersionParameters.t -> Version.t -> 'w), 'w, Void.t) t
| PreviewOgg : ((VersionParameters.t -> Version.t -> 'w), 'w, Void.t) t

let to_string : type a w r. (a, w, r) t -> string = function
  | Create -> "Create"
  | Search -> "Search"
  | Get -> "Get"
  | Update -> "Update"
  | Ly -> "Ly"
  | Svg -> "Svg"
  | Ogg -> "Ogg"
  | Pdf -> "Pdf"
  | PreviewSvg -> "PreviewSvg"
  | PreviewOgg -> "PreviewOgg"

(* FIXME: make a simple PPX for the following *)
type wrapped = W : ('a, 'r Lwt.t, 'r) t -> wrapped
let all = [
  W Search;
  W Create;
  W Update;
  W Ly;
  W Svg;
  W Ogg;
  W Pdf;
  W PreviewSvg;
  W PreviewOgg;
  (* WARNING: THE ORDER MATTERS *)
  W Get;
]

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

let route : type a w r. (a, w, r) t -> (a, w, r) route = function
  (* Actions without specific version *)
  | Create -> query "version" (module Version) @@ post (module Entry.J(VersionNoContent))
  | Search -> query "slice" (module Slice) @@ query "filter" (module Version.Filter) @@ get (module JPair(JInt)(JList(Entry.J(VersionNoContent))))
  (* Actions on a specific version *)
  | Get -> variable (module SSlug(Version)) @@ get (module Entry.J(VersionNoContent))
  | Update -> variable (module SSlug(Version)) @@ query "version" (module Version) @@ put (module Entry.J(VersionNoContent))
  (* Files related to a version *)
  | Ly -> variable (module SSlug(Version)) ~suffix: ".ly" @@ void ()
  | Svg -> query "parameters" (module VersionParameters) @@ variable (module SSlug(Version)) ~suffix: ".svg" @@ void ()
  | Ogg -> query "parameters" (module VersionParameters) @@ variable (module SSlug(Version)) ~suffix: ".ogg" @@ void ()
  | Pdf -> query "parameters" (module VersionParameters) @@ variable (module SSlug(Version)) ~suffix: ".pdf" @@ void ()
  (* Files related to an anonymous version *)
  | PreviewSvg -> query "parameters" (module VersionParameters) @@ query "version" (module Version) @@ literal "preview.svg" @@ void ()
  | PreviewOgg -> query "parameters" (module VersionParameters) @@ query "version" (module Version) @@ literal "preview.ogg" @@ void ()
