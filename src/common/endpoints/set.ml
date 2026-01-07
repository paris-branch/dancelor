open Nes
open Madge
open ModelBuilder.Core
module Filter = FilterBuilder.Core

type (_, _, _) t =
(* Actions without specific set *)
| Create : ((Set.t -> Entry.Access.Private.t -> 'w), 'w, Set.entry) t
| Search : ((Slice.t -> Filter.Set.t -> 'w), 'w, (int * Set.entry list)) t
(* Actions on a specific set *)
| Get : ((Set.t Entry.Id.t -> 'w), 'w, Set.entry) t
| Update : ((Set.t Entry.Id.t -> Set.t -> Entry.Access.Private.t -> 'w), 'w, Set.entry) t
| Delete : ((Set.t Entry.Id.t -> 'w), 'w, unit) t
(* Files related to a set *)
| BuildPdf : ((Set.t Entry.Id.t -> SetParameters.t -> RenderingParameters.t -> 'w), 'w, Job_id.t Job.registration_response) t
[@@deriving madge_wrapped_endpoints]

let route : type a w r. (a, w, r) t -> (a, w, r) route =
  let open Route in
  function
    (* Actions without specific set *)
    | Create -> body "set" (module Set) @@ body "access" (module Entry.Access.Private) @@ post (module Entry.JPrivate(Set))
    | Search -> query "slice" (module Slice) @@ query "filter" (module Filter.Set) @@ get (module JPair(JInt)(JList(Entry.JPrivate(Set))))
    (* Actions on a specific set *)
    | Get -> variable (module Entry.Id.S(Set)) @@ get (module Entry.JPrivate(Set))
    | Update -> variable (module Entry.Id.S(Set)) @@ body "set" (module Set) @@ body "access" (module Entry.Access.Private) @@ put (module Entry.JPrivate(Set))
    | Delete -> variable (module Entry.Id.S(Set)) @@ delete (module JUnit)
    (* Files related to a set *)
    | BuildPdf -> literal "build-pdf" @@ variable (module Entry.Id.S(Set)) @@ query "parameters" (module SetParameters) @@ query "rendering-parameters" (module RenderingParameters) @@ post (module Job.Registration_response(Job_id))
