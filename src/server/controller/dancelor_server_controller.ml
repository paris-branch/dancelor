module Person = Person
module Set = Set
module Book = Book
module Dance = Dance
module Version = Version
module Tune = Tune

open Dancelor_common

let dispatch : type a r. (a, r Lwt.t, r) ApiRouter.endpoint_new -> a = function
  | Person endpoint -> Person.dispatch endpoint
  | Book endpoint -> Book.dispatch endpoint
  | Set endpoint -> Set.dispatch endpoint
  | Tune endpoint -> Tune.dispatch endpoint
