open Common

module Source = Source
module Person = Person
module Set = Set
module Book = Book
module Dance = Dance
module Version = Version
module Tune = Tune

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
  | ReportIssue -> IssueReport.report env
  | Victor -> Logger.log_exit (module Log) 101
