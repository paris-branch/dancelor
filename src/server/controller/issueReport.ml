open Nes
module Model = Dancelor_server_model
open Model.IssueReport
open Request

let describe =
  Dancelor_common.PageRouter.make_describe
    ~get_version: Model.Version.get
    ~get_tune: Model.Tune.get
    ~get_set: Model.Set.get
    ~get_book: Model.Book.get
    ~get_dance: Model.Dance.get
    ~get_person: Model.Person.get

let report issue =
  let%lwt (repo, title) =
    if issue.source_is_dancelor then
      Lwt.return (!Dancelor_server_config.github_repository, issue.title)
    else
      let%lwt (model, name) = Lwt.map Option.get @@ describe @@ Uri.of_string issue.page in
      Lwt.return (!Dancelor_server_config.github_database_repository, Format.sprintf "%s “%s”: %s" model name issue.title)
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
        "GH_TOKEN=" ^ !Dancelor_server_config.github_token;
      |]
      ~on_wrong_status: Logs.Error
      ~on_nonempty_stderr: Logs.Error
      ["gh"; "issue"; "create"; "--repo"; repo; "--title"; title; "--body"; body]
  in
  let uri = String.trim output.stdout in
  let id = int_of_string @@ String.remove_prefix_exn ~needle: ("/" ^ repo ^ "/issues/") @@ Uri.(path % of_string) uri in
  Lwt.return Response.{title; id; uri}
