open Nes
open Madge
open Database
open Model

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

(* FIXME: make a simple PPX for the following *)
type wrapped = W : ('a, 'r Lwt.t, 'r) t -> wrapped
let all = [W Get; W Search; W Create; W Update; W Ly; W Svg; W Ogg; W Pdf]

(* NOTE: The version model contains its LilyPond content. This is a big string
   that is not used in the client. It would be better to have a clean way to
   describe fields that are not included by default, but for now we will just
   redact it from the HTTP responses. *)
module JVersion = struct
  type t = Version.t
  let of_yojson = Version.of_yojson % Json.add_field "content" (`String "")
  let to_yojson = Json.remove_field "content" % Version.to_yojson
end

let route : type a w r. (a, w, r) t -> (a, w, r) route = function
  (* Actions without specific version *)
  | Create -> query "version" (module JVersion) @@ post (module Entry.J(JVersion))
  | Search -> query "slice" (module Slice) @@ query "filter" (module Version.Filter) @@ get (module JPair(JInt)(JList(Entry.J(JVersion))))
  (* Actions on a specific version *)
  | Get -> variable (module SSlug(JVersion)) @@ get (module Entry.J(JVersion))
  | Update -> variable (module SSlug(JVersion)) @@ query "version" (module JVersion) @@ put (module Entry.J(JVersion))
  (* Files related to a version *)
  | Ly -> variable (module SSlug(JVersion)) ~suffix: ".ly" @@ void ()
  | Svg -> query "parameters" (module VersionParameters) @@ variable (module SSlug(JVersion)) ~suffix: ".svg" @@ void ()
  | Ogg -> query "parameters" (module VersionParameters) @@ variable (module SSlug(JVersion)) ~suffix: ".ogg" @@ void ()
  | Pdf -> query "parameters" (module VersionParameters) @@ variable (module SSlug(JVersion)) ~suffix: ".pdf" @@ void ()
