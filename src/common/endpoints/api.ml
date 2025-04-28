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
  | Auth : ('a, 'w, 'r) Auth.t -> ('a, 'w, 'r) t
  | ReportIssue : (IssueReport.request -> 'w, 'w, IssueReport.response) t
  | Victor : ('w, 'w, Void.t) t

let to_string : type a w r. (a, w, r) t -> string = function
  | Source endpoint -> "Source " ^ Source.to_string endpoint
  | Person endpoint -> "Person " ^ Person.to_string endpoint
  | Book endpoint -> "Book " ^ Book.to_string endpoint
  | Version endpoint -> "Version " ^ Version.to_string endpoint
  | Dance endpoint -> "Dance " ^ Dance.to_string endpoint
  | Set endpoint -> "Set " ^ Set.to_string endpoint
  | Tune endpoint -> "Tune " ^ Tune.to_string endpoint
  | Any endpoint -> "Any " ^ Any.to_string endpoint
  | Auth endpoint -> "Auth " ^ Auth.to_string endpoint
  | ReportIssue -> "ReportIssue"
  | Victor -> "Victor"

type wrapped =
  | W : ('a, 'r Lwt.t, 'r) t -> wrapped

let all_endpoints =
  List.flatten
    [
      List.map (fun (Source.W e) -> W (Source e)) Source.all;
      List.map (fun (Person.W e) -> W (Person e)) Person.all;
      List.map (fun (Book.W e) -> W (Book e)) Book.all;
      List.map (fun (Version.W e) -> W (Version e)) Version.all;
      List.map (fun (Dance.W e) -> W (Dance e)) Dance.all;
      List.map (fun (Set.W e) -> W (Set e)) Set.all;
      List.map (fun (Tune.W e) -> W (Tune e)) Tune.all;
      List.map (fun (Any.W e) -> W (Any e)) Any.all;
      List.map (fun (Auth.W e) -> W (Auth e)) Auth.all;
      [W ReportIssue;
      W Victor];
    ]

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
    | Auth endpoint -> literal "api" @@ literal "auth" @@ Auth.route endpoint
    | ReportIssue -> literal "api" @@ literal "issue" @@ literal "report" @@ query "request" (module IssueReport.Request) @@ post (module IssueReport.Response)
    | Victor -> literal "api" @@ literal "victor" @@ void ()

let href : type a r. (a, string, r) t -> a = fun endpoint ->
  with_request (route endpoint) @@ fun (module _) {meth; uri; _} ->
  assert (meth = GET); Uri.to_string uri
