open Nes
open Madge

module Endpoints = struct
  type (_, _, _) t =
    | Source : ('a, 'w, 'r) Source.t -> ('a, 'w, 'r) t
    | Person : ('a, 'w, 'r) Person.t -> ('a, 'w, 'r) t
    | Book : ('a, 'w, 'r) Book.t -> ('a, 'w, 'r) t
    | Version : ('a, 'w, 'r) Version.t -> ('a, 'w, 'r) t
    | Dance : ('a, 'w, 'r) Dance.t -> ('a, 'w, 'r) t
    | Set : ('a, 'w, 'r) Set.t -> ('a, 'w, 'r) t
    | Tune : ('a, 'w, 'r) Tune.t -> ('a, 'w, 'r) t
    | Any : ('a, 'w, 'r) Any.t -> ('a, 'w, 'r) t
    | User : ('a, 'w, 'r) User.t -> ('a, 'w, 'r) t
    | Job : ('a, 'w, 'r) Job.t -> ('a, 'w, 'r) t
    | ReportIssue : (IssueReport.request -> 'w, 'w, IssueReport.response) t
    | Victor : ('w, 'w, Void.t) t
    | BootTime : ('w, 'w, Datetime.t) t
    | Metrics : ('w, 'w, Void.t) t
  [@@deriving madge_wrapped_endpoints]
end
include Endpoints

include Madge.Make_endpoints(struct
  include Endpoints

  let route_internal : type a w r. (a, w, r) t -> (a, w, r) route =
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
end)

let href : type a r. (a, string, r) t -> a = fun endpoint ->
  with_request (route endpoint) @@ fun (module _) request ->
  assert (Request.meth request = GET);
  Uri.to_string (Request.uri request)
