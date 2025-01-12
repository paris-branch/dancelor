open Nes
module Model = Dancelor_server_model
open Model.IssueReport
open Request

let describe = Dancelor_common.PageRouter.make_describe
    ~get_version: Model.Version.get
    ~get_tune: Model.Tune.get
    ~get_set: Model.Set.get
    ~get_book: Model.Book.get
    ~get_dance: Model.Dance.get
    ~get_person: Model.Person.get

let report issue =
  let%lwt (repo, title) =
    if issue.source_is_dancelor then
      Lwt.return ("paris-branch/test-dancelor", issue.title)
    else
      let%lwt (model, name) = Lwt.map Option.get @@ describe @@ Uri.of_string issue.page in
      Lwt.return ("paris-branch/test-dancelor-database", Format.sprintf "%s “%s”: %s" model name issue.title)
  in
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
  let%lwt output = NesProcess.run ["gh"; "issue"; "create"; "--repo"; repo; "--title"; title; "--body"; body] in
  let uri = String.trim output.stdout in
  let id = int_of_string @@ String.remove_prefix_exn ~needle: ("/" ^ repo ^ "/issues/") @@ Uri.(path % of_string) uri in
  Lwt.return Response.{title; id; uri}
