open Nes
open Common

open Endpoints.IssueReport
open Request

let describe =
  Endpoints.Page.make_describe
    ~get_version: Model.Version.get
    ~get_tune: Model.Tune.get
    ~get_set: Model.Set.get
    ~get_book: Model.Book.get
    ~get_dance: Model.Dance.get
    ~get_person: Model.Person.get

(* used at the end of the {!report} function below *)
let id_regexp = Str.regexp ".*/issues/\\(.*\\)"

let report issue =
  let%lwt (repo, title) =
    if issue.source_is_dancelor then
      Lwt.return (!Config.github_repository, issue.title)
    else
      let%lwt (model, name) = Lwt.map Option.get @@ describe @@ Uri.of_string issue.page in
      Lwt.return (!Config.github_database_repository, Format.sprintf "%s “%s”: %s" model name issue.title)
  in
  assert (repo <> "");
  (* otherwise this will pick up on the current Git repository *)
  let body =
    Format.sprintf
      {|
**Reporter**: %s

**Page**: %s

**Description**:

%s
          |}
      issue.reporter
      issue.page
      issue.description
  in
  let%lwt output =
    NesProcess.run
      ~env: [|
        "PATH=" ^ (Unix.getenv "PATH");
        "GH_TOKEN=" ^ !Config.github_token;
      |]
      ~on_wrong_status: Logs.Error
      ~on_nonempty_stderr: Logs.Error
      ["gh"; "issue"; "create"; "--repo"; repo; "--title"; title; "--body"; body]
  in
  let uri = String.trim output.stdout in
  assert (Str.string_match id_regexp uri 0);
  let id = int_of_string @@ Str.matched_group 1 uri in
  Lwt.return Response.{title; id; uri}
