module Person = Person
module Set = Set
module Book = Book
module Dance = Dance
module Version = Version

open Dancelor_common

let dispatch : type a r. (a, r Lwt.t, r) ApiRouter.endpoint_new -> a = function
  | Person endpoint -> Person.dispatch endpoint
