open Nes
open Dancelor_common_model_utils
open Dancelor_common_model_endpoints

type (_, _, _) endpoint =
  | Person : ('a, 'w, 'r) Person.t -> ('a, 'w, 'r) endpoint
  | Book : ('a, 'w, 'r) Book.t -> ('a, 'w, 'r) endpoint
  | Version : ('a, 'w, 'r) Version.t -> ('a, 'w, 'r) endpoint
  | Dance : ('a, 'w, 'r) Dance.t -> ('a, 'w, 'r) endpoint
  | Set : ('a, 'w, 'r) Set.t -> ('a, 'w, 'r) endpoint
  | Tune : ('a, 'w, 'r) Tune.t -> ('a, 'w, 'r) endpoint
  | Any : ('a, 'w, 'r) Any.t -> ('a, 'w, 'r) endpoint
  | ReportIssue : (IssueReport.request -> 'w, 'w, IssueReport.response) endpoint
  | Victor : ('w, 'w, Void.t) endpoint

type endpoint_wrapped =
  | W : ('a, 'r Lwt.t, 'r) endpoint -> endpoint_wrapped

let all_endpoints =
  List.flatten
    [
      List.map (fun (Person.W e) -> W (Person e)) Person.all;
      List.map (fun (Book.W e) -> W (Book e)) Book.all;
      List.map (fun (Version.W e) -> W (Version e)) Version.all;
      List.map (fun (Dance.W e) -> W (Dance e)) Dance.all;
      List.map (fun (Set.W e) -> W (Set e)) Set.all;
      List.map (fun (Tune.W e) -> W (Tune e)) Tune.all;
      List.map (fun (Any.W e) -> W (Any e)) Any.all;
      [W ReportIssue; W Victor];
    ]

open Madge

(* FIXME: Factorise adding the `/api` prefix. *)
let route : type a w r. (a, w, r) endpoint -> (a, w, r) route = function
  | Person endpoint -> literal "api" @@ literal "person" @@ Person.route endpoint
  | Book endpoint -> literal "api" @@ literal "book" @@ Book.route endpoint
  | Version endpoint -> literal "api" @@ literal "version" @@ Version.route endpoint
  | Dance endpoint -> literal "api" @@ literal "dance" @@ Dance.route endpoint
  | Set endpoint -> literal "api" @@ literal "set" @@ Set.route endpoint
  | Tune endpoint -> literal "api" @@ literal "tune" @@ Tune.route endpoint
  | Any endpoint -> literal "api" @@ literal "any" @@ Any.route endpoint
  | ReportIssue -> literal "api" @@ literal "issue" @@ literal "report" @@ query "request" (module IssueReport.Request) @@ post (module IssueReport.Response)
  | Victor -> literal "api" @@ literal "victor" @@ void ()

let href : type a r. (a, string, r) endpoint -> a = fun endpoint ->
  process (route endpoint) (fun (module _) {meth; uri; _} -> assert (meth = GET); Uri.to_string uri)
