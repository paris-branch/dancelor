open Nes
open Madge

(* NOTE: The user model contains passwords and other secret tokens. Even though
   they are heavily hashed, we shouldn't be sending them, so we systematically
   redact them. And to be sure to not miss a future introduction of  *)
module User = struct
  type t = ModelBuilder.Core.User.t
  let to_yojson = Json.keep_fields ["username"; "person"] % ModelBuilder.Core.User.to_yojson
  let of_yojson = ModelBuilder.Core.User.of_yojson
end

type (_, _, _) t =
  | Status : ('w, 'w, User.t Entry.t option) t
  | CanCreate : ('w, 'w, bool) t
  | CanAdmin : ('w, 'w, bool) t
  | SignIn : ((string -> string -> bool -> 'w), 'w, User.t Entry.t option) t
  | SignOut : ('w, 'w, unit) t
  | Create : ((User.t -> 'w), 'w, User.t Entry.t * string) t
  | ResetPassword : ((string -> string -> string -> 'w), 'w, unit) t
[@@deriving madge_wrapped_endpoints]

let route : type a w r. (a, w, r) t -> (a, w, r) route =
  let open Route in
  function
    | Status -> literal "status" @@ post (module JOption(Entry.J(User)))
    | SignIn -> literal "sign-in" @@ body "username" (module JString) @@ body "password" (module JString) @@ body "remember-me" (module JBool) @@ post (module JOption(Entry.J(User)))
    | SignOut -> literal "sign-out" @@ post (module JUnit)
    | Create -> literal "create" @@ body "user" (module User) @@ post (module JPair(Entry.J(User))(JString))
    | ResetPassword -> literal "reset-password" @@ body "username" (module JString) @@ body "token" (module JString) @@ body "password" (module JString) @@ post (module JUnit)
    | CanCreate -> literal "can-create" @@ post (module JBool)
    | CanAdmin -> literal "can-admin" @@ post (module JBool)
