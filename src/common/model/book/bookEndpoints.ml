open Nes
open Madge_common

(* Old-style Endpoints *)
(* FIXME: to be converted into new-style ones. *)

module Arguments = struct
  let slug = arg ~key: "slug" (module MSlug(BookCore))
  let status = optarg (module Status)
  let title = arg ~key: "title" (module MString)
  let date = optarg ~key: "date" (module NesPartialDate)
  let contents = optarg ~key: "contents" (module MList(BookCore.PageCore))
  let filter = arg (module BookCore.Filter)
  let slice = optarg (module Slice)
  let threshold = optarg ~key: "threshold" (module MFloat)
  let modified_at = arg ~key: "modified-at" (module NesDatetime)
  let created_at = arg ~key: "created-at" (module NesDatetime)
end

let get = endpoint ~path: "/book" (module BookCore)
let make_and_save = endpoint ~path: "/book/save" (module BookCore)
let search = endpoint ~path: "/book/search" (module MPair(MInteger)(MList(BookCore)))
let update = endpoint ~path: "/book/update" (module MUnit)

(* New-style Endpoints *)

open Madge

type (_, _, _) t =
  | Pdf : ((BookParameters.t option -> string -> 'w), 'w, MVoid.t) t

type wrapped = W : ('a, 'r Lwt.t, 'r) t -> wrapped
let all = [W Pdf]

let route : type a w r. (a, w, r) t -> (a, w, r) route = function
  | Pdf -> literal "pdf" @@ query_opt "parameters" (module BookParameters) @@ variable (module SString) @@ return (module JVoid)
