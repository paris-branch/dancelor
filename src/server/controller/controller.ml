open Nes
open Common

module Source = Source
module Person = Person
module Set = Set
module Book = Book
module Dance = Dance
module Version = Version
module Tune = Tune
module Job = Job
module Metrics = Metrics

module Log = (val Logger.create "controller": Logs.LOG)

let dispatch : type a r. Environment.t -> (a, r Lwt.t, r) Endpoints.Api.t -> a = fun env endpoint ->
  match endpoint with
  | Source endpoint -> Source.dispatch env endpoint
  | Person endpoint -> Person.dispatch env endpoint
  | Book endpoint -> Book.dispatch env endpoint
  | Version endpoint -> Version.dispatch env endpoint
  | Dance endpoint -> Dance.dispatch env endpoint
  | Set endpoint -> Set.dispatch env endpoint
  | Tune endpoint -> Tune.dispatch env endpoint
  | Any endpoint -> Any.dispatch env endpoint
  | User endpoint -> User.dispatch env endpoint
  | Job endpoint -> Job.dispatch env endpoint
  | ReportIssue -> IssueReport.report env
  | Victor ->
    Log.debug (fun m -> m "Triggering controller for Victor");
    Permission.assert_is_connected env;%lwt
    (* FIXME: eventually, only for database editors *)
    Logger.log_exit (module Log) 101
  | BootTime ->
    Log.debug (fun m -> m "Answering boot time");
    lwt Environment.boot_time
