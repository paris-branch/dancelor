open Nes
open Madge
open ModelBuilder

type (_, _, _) t =
  | Status : ('w, 'w, Person.t Entry.t option) t
  | Login : ((string -> string -> 'w), 'w, Person.t Entry.t option) t
  | Logout : ('w, 'w, unit) t
  | ResetPassword : ((string -> string -> string -> 'w), 'w, unit) t

let to_string : type a w r. (a, w, r) t -> string = function
  | Status -> "Status"
  | Login -> "Login"
  | Logout -> "Logout"
  | ResetPassword -> "ResetPassword"

type wrapped = W : ('a, 'r Lwt.t, 'r) t -> wrapped
let all = [W Status; W Login; W Logout; W ResetPassword]

let route : type a w r. (a, w, r) t -> (a, w, r) route = function
  | Status -> literal "status" @@ post (module JOption(Entry.J(Person)))
  | Login -> literal "login" @@ body "username" (module JString) @@ body "password" (module JString) @@ post (module JOption(Entry.J(Person)))
  | Logout -> literal "logout" @@ post (module JUnit)
  | ResetPassword -> literal "reset-password" @@ body "username" (module JString) @@ body "token" (module JString) @@ body "password" (module JString) @@ post (module JUnit)
