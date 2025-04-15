open Nes
open Madge
open ModelBuilder

(* FIXME: Rename user -> auth *)

type (_, _, _) auth =
  | Status : ('w, 'w, Person.t Entry.t option) auth
  | Login : ((string -> string -> 'w), 'w, Person.t Entry.t option) auth
  | Logout : ('w, 'w, unit) auth

type (_, _, _) t =
  | Auth : ('a, 'w, 'r) auth -> ('a, 'w, 'r) t

let to_string : type a w r. (a, w, r) t -> string = function
  | Auth Status -> "Auth Status"
  | Auth Login -> "Auth Login"
  | Auth Logout -> "Auth Logout"

type wrapped = W : ('a, 'r Lwt.t, 'r) t -> wrapped
let all = [W (Auth Status); W (Auth Login); W (Auth Logout)]

let route : type a w r. (a, w, r) t -> (a, w, r) route = function
  | Auth Status -> literal "auth" @@ literal "status" @@ post (module JOption(Entry.J(Person)))
  | Auth Login -> literal "auth" @@ literal "login" @@ query "username" (module JString) @@ query "password" (module JString) @@ post (module JOption(Entry.J(Person)))
  (* FIXME: at this point, the password travels in clear in the URL. This is
     WRONG. Improve Madge to support body query arguments. *)
  | Auth Logout -> literal "auth" @@ literal "logout" @@ post (module JUnit)
