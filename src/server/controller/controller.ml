open Common

module Source = Source
module Person = Person
module Set = Set
module Book = Book
module Dance = Dance
module Version = Version
module Tune = Tune

module Log = (val Logger.create "controller": Logs.LOG)

let dispatch : type a r. (a, r Lwt.t, r) Endpoints.Api.t -> a = function
  | Source endpoint -> Source.dispatch endpoint
  | Person endpoint -> Person.dispatch endpoint
  | Book endpoint -> Book.dispatch endpoint
  | Version endpoint -> Version.dispatch endpoint
  | Dance endpoint -> Dance.dispatch endpoint
  | Set endpoint -> Set.dispatch endpoint
  | Tune endpoint -> Tune.dispatch endpoint
  | Any endpoint -> Any.dispatch endpoint
  | ReportIssue -> IssueReport.report
  | Victor -> Logger.log_exit (module Log) 101
