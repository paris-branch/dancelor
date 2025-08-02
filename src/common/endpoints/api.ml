open Nes

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
  | ReportIssue : (IssueReport.request -> 'w, 'w, IssueReport.response) t
  | Victor : ('w, 'w, Void.t) t
  | BootTime : ('w, 'w, Datetime.t) t
[@@deriving madge_wrapped_endpoints]

open Madge

(* FIXME: Factorise adding the `/api` prefix. *)
let route : type a w r. (a, w, r) t -> (a, w, r) route =
  let open Route in
  function
    | Source endpoint -> literal "api" @@ literal "source" @@ Source.route endpoint
    | Person endpoint -> literal "api" @@ literal "person" @@ Person.route endpoint
    | Book endpoint -> literal "api" @@ literal "book" @@ Book.route endpoint
    | Version endpoint -> literal "api" @@ literal "version" @@ Version.route endpoint
    | Dance endpoint -> literal "api" @@ literal "dance" @@ Dance.route endpoint
    | Set endpoint -> literal "api" @@ literal "set" @@ Set.route endpoint
    | Tune endpoint -> literal "api" @@ literal "tune" @@ Tune.route endpoint
    | Any endpoint -> literal "api" @@ literal "any" @@ Any.route endpoint
    | User endpoint -> literal "api" @@ literal "user" @@ User.route endpoint
    | ReportIssue -> literal "api" @@ literal "issue" @@ literal "report" @@ query "request" (module IssueReport.Request) @@ post (module IssueReport.Response)
    | Victor -> literal "api" @@ literal "victor" @@ void ()
    | BootTime -> literal "api" @@ literal "boot-time" @@ get (module Datetime)

let href : type a r. (a, string, r) t -> a = fun endpoint ->
  with_request (route endpoint) @@ fun (module _) {meth; uri; _} ->
  assert (meth = GET); Uri.to_string uri
