open Nes
open Madge
open Dancelor_common_database

type (_, _, _) t =
  (* Actions without specific version *)
  | Create : ((VersionCore.t -> 'w), 'w, VersionCore.t Entry.t) t
  | Search : ((Slice.t -> VersionCore.Filter.t -> 'w), 'w, (int * VersionCore.t Entry.t list)) t
  (* Actions on a specific version *)
  | Get : ((VersionCore.t Slug.t -> 'w), 'w, VersionCore.t Entry.t) t
  | Update : ((VersionCore.t Slug.t -> VersionCore.t -> 'w), 'w, VersionCore.t Entry.t) t
  (* Files related to a version *)
  | Ly : ((VersionCore.t Slug.t -> 'w), 'w, Void.t) t
  | Svg : ((VersionParameters.t -> VersionCore.t Slug.t -> 'w), 'w, Void.t) t
  | Ogg : ((VersionParameters.t -> VersionCore.t Slug.t -> 'w), 'w, Void.t) t
  | Pdf : ((VersionParameters.t -> VersionCore.t Slug.t -> 'w), 'w, Void.t) t

(* FIXME: make a simple PPX for the following *)
type wrapped = W : ('a, 'r Lwt.t, 'r) t -> wrapped
let all = [W Get; W Search; W Create; W Update; W Ly; W Svg; W Ogg; W Pdf]

(* NOTE: The version model contains its LilyPond content. This is a big string
   that is not used in the client. It would be better to have a clean way to
   describe fields that are not included by default, but for now we will just
   redact it from the HTTP responses. *)
module JVersionCore = struct
  type t = VersionCore.t
  let of_yojson = VersionCore.of_yojson % Json.add_field "content" (`String "")
  let to_yojson = Json.remove_field "content" % VersionCore.to_yojson
end

let route : type a w r. (a, w, r) t -> (a, w, r) route = function
  (* Actions without specific version *)
  | Create -> query "version" (module JVersionCore) @@ post (module Entry.J(JVersionCore))
  | Search -> query "slice" (module Slice) @@ query "filter" (module VersionCore.Filter) @@ get (module JPair(JInt)(JList(Entry.J(JVersionCore))))
  (* Actions on a specific version *)
  | Get -> variable (module SSlug(JVersionCore)) @@ get (module Entry.J(JVersionCore))
  | Update -> variable (module SSlug(JVersionCore)) @@ query "version" (module JVersionCore) @@ put (module Entry.J(JVersionCore))
  (* Files related to a version *)
  | Ly -> variable (module SSlug(JVersionCore)) ~suffix: ".ly" @@ void ()
  | Svg -> query "parameters" (module VersionParameters) @@ variable (module SSlug(JVersionCore)) ~suffix: ".svg" @@ void ()
  | Ogg -> query "parameters" (module VersionParameters) @@ variable (module SSlug(JVersionCore)) ~suffix: ".ogg" @@ void ()
  | Pdf -> query "parameters" (module VersionParameters) @@ variable (module SSlug(JVersionCore)) ~suffix: ".pdf" @@ void ()
