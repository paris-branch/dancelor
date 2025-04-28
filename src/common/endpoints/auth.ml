open Nes
open Madge
open ModelBuilder

type (_, _, _) t =
  | Status : ('w, 'w, Person.t Entry.t option) t
  | Login : ((string -> string -> bool -> 'w), 'w, Person.t Entry.t option) t
  | Logout : ('w, 'w, unit) t
  | CreateUser : ((string -> Person.t Slug.t -> 'w), 'w, string) t
  | ResetPassword : ((string -> string -> string -> 'w), 'w, unit) t
[@@deriving madge_wrapped_endpoints]

let route : type a w r. (a, w, r) t -> (a, w, r) route =
  let open Route in
  function
    | Status -> literal "status" @@ post (module JOption(Entry.J(Person)))
    | Login -> literal "login" @@ body "username" (module JString) @@ body "password" (module JString) @@ body "remember-me" (module JBool) @@ post (module JOption(Entry.J(Person)))
    | Logout -> literal "logout" @@ post (module JUnit)
    | CreateUser -> literal "create-user" @@ body "username" (module JString) @@ body "person" (module JSlug(Person)) @@ post (module JString)
    | ResetPassword -> literal "reset-password" @@ body "username" (module JString) @@ body "token" (module JString) @@ body "password" (module JString) @@ post (module JUnit)
