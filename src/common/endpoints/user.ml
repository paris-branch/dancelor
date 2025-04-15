open Nes
open Madge
open ModelBuilder

type (_, _, _) auth =
  | Status : ('w, 'w, User.t Entry.t option) auth
  | Login : ((string -> string -> 'w), 'w, User.t Entry.t option) auth
  | Logout : ('w, 'w, unit) auth

type (_, _, _) t =
  | Get : ((User.t Slug.t -> 'w), 'w, User.t Entry.t) t
  | Auth : ('a, 'w, 'r) auth -> ('a, 'w, 'r) t

let to_string : type a w r. (a, w, r) t -> string = function
  | Get -> "Get"
  | Auth Status -> "Auth Status"
  | Auth Login -> "Auth Login"
  | Auth Logout -> "Auth Logout"

type wrapped = W : ('a, 'r Lwt.t, 'r) t -> wrapped
let all = [W Get; W (Auth Status); W (Auth Login); W (Auth Logout)]

let route : type a w r. (a, w, r) t -> (a, w, r) route = function
  | Get -> variable (module SSlug(User)) @@ get (module Entry.J(User))
  (* FIXME: at this point, the [Get] endpoint can return passwords. Even if
     they are hashed, this is really not good. *)
  | Auth Status -> literal "auth" @@ literal "status" @@ post (module JOption(Entry.J(User)))
  | Auth Login -> literal "auth" @@ literal "login" @@ query "username" (module JString) @@ query "password" (module JString) @@ post (module JOption(Entry.J(User)))
  (* FIXME: at this point, the password travels in clear in the URL. This is
     WRONG. Improve Madge to support body query arguments. *)
  | Auth Logout -> literal "auth" @@ literal "logout" @@ post (module JUnit)
