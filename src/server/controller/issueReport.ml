open Nes
open Common

open Endpoints.IssueReport
open Request

include Endpoints.Page.MakeDescribe(Model)

(* used at the end of the {!report} function below *)
let id_regexp = Str.regexp ".*/issues/\\(.*\\)"

let report _env issue =
  let%lwt (repo, title) =
    if issue.source_is_dancelor then
      lwt (!Config.github_repository, issue.title)
    else
      let%lwt (model, name) = Option.get <$> describe @@ Uri.of_string issue.page in
      lwt (!Config.github_database_repository, Format.sprintf "%s “%s”: %s" model name issue.title)
  in
  assert (repo <> "");
  (* otherwise this will pick up on the current Git repository *)
  let body =
    spf
      "**Reporter**: %s\n\n**Page**: %s\n\n**Description**:\n\n%s\n"
      (
        match issue.reporter with
        | Left user -> NEString.to_string @@ Model.User.username' user (* FIXME: when there is a profile page for users, link to it *)
        | Right string -> string
      )
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
  lwt Response.{title; id; uri}
