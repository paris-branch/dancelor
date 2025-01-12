module Person = Person
module Set = Set
module Book = Book
module Dance = Dance
module Version = Version
module Tune = Tune
module Log = (val Dancelor_server_logs.create "controller": Logs.LOG)

open Dancelor_common

let dispatch : type a r. (a, r Lwt.t, r) ApiRouter.endpoint -> a = function
  | Person endpoint -> Person.dispatch endpoint
  | Book endpoint -> Book.dispatch endpoint
  | Version endpoint -> Version.dispatch endpoint
  | Dance endpoint -> Dance.dispatch endpoint
  | Set endpoint -> Set.dispatch endpoint
  | Tune endpoint -> Tune.dispatch endpoint
  | Any endpoint -> Any.dispatch endpoint
  | ReportIssue -> IssueReport.report
  | Victor -> Dancelor_server_logs.log_exit (module Log) 101
