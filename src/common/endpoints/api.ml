open Nes
open Madge

(* Internal API *)

type (_, _, _) internal =
  | Source : ('a, 'w, 'r) Source.t -> ('a, 'w, 'r) internal
  | Person : ('a, 'w, 'r) Person.t -> ('a, 'w, 'r) internal
  | Book : ('a, 'w, 'r) Book.t -> ('a, 'w, 'r) internal
  | Version : ('a, 'w, 'r) Version.t -> ('a, 'w, 'r) internal
  | Dance : ('a, 'w, 'r) Dance.t -> ('a, 'w, 'r) internal
  | Set : ('a, 'w, 'r) Set.t -> ('a, 'w, 'r) internal
  | Tune : ('a, 'w, 'r) Tune.t -> ('a, 'w, 'r) internal
  | Any : ('a, 'w, 'r) Any.t -> ('a, 'w, 'r) internal
  | User : ('a, 'w, 'r) User.t -> ('a, 'w, 'r) internal
  | Job : ('a, 'w, 'r) Job.t -> ('a, 'w, 'r) internal
  | ReportIssue : (IssueReport.request -> 'w, 'w, IssueReport.response) internal
  | Victor : ('w, 'w, Void.t) internal
  | BootTime : ('w, 'w, Datetime.t) internal
  | Metrics : ('w, 'w, Void.t) internal
[@@deriving madge_wrapped_endpoints]

(* This is internal in the sense that it does not care about the /api prefix. It
   should not be exposed and used. *)
let route_internal : type a w r. (a, w, r) internal -> (a, w, r) route =
  let open Route in
  function
    | Source endpoint -> literal "source" @@ Source.route endpoint
    | Person endpoint -> literal "person" @@ Person.route endpoint
    | Book endpoint -> literal "book" @@ Book.route endpoint
    | Version endpoint -> literal "version" @@ Version.route endpoint
    | Dance endpoint -> literal "dance" @@ Dance.route endpoint
    | Set endpoint -> literal "set" @@ Set.route endpoint
    | Tune endpoint -> literal "tune" @@ Tune.route endpoint
    | Any endpoint -> literal "any" @@ Any.route endpoint
    | User endpoint -> literal "user" @@ User.route endpoint
    | Job endpoint -> literal "job" @@ Job.route endpoint
    | ReportIssue -> literal "issue" @@ literal "report" @@ query "request" (module IssueReport.Request) @@ post (module IssueReport.Response)
    | Victor -> literal "victor" @@ void ()
    | BootTime -> literal "boot-time" @@ get (module Datetime)
    | Metrics -> literal "metrics" @@ void ()

(* Full API *)

type (_, _, _) full =
  | Api : ('a, 'w, 'r) internal -> ('a, 'w, 'r) full
  | Batch : (Request.t list -> 'w, 'w, Response.t list) full
[@@deriving madge_wrapped_endpoints]

let route_full : type a w r. (a, w, r) full -> (a, w, r) route =
  let open Route in
  function
    | Api endpoint -> literal "api" @@ route_internal endpoint
    | Batch -> literal "api" @@ literal "batch" @@ body "requests" (module JList(Request)) @@ post (module JList(Response))

(* Lifted internal API *)

let name = internal_name

let route endpoint = route_full (Api endpoint)

let href : type a r. (a, string, r) internal -> a = fun endpoint ->
  with_request (route endpoint) @@ fun (module _) request ->
  assert (Request.meth request = GET);
  Uri.to_string (Request.uri request)
