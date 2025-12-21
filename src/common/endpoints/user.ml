open Nes
open Madge

(* NOTE: The user model contains passwords and other secret tokens. Even though
   they are heavily hashed, we shouldn't be sending them, so we systematically
   redact them. And to be sure to not miss a future introduction of  *)
module User = struct
  type t = ModelBuilder.Core.User.t
  [@@deriving of_biniou, of_yojson]

  type proxy = {
    username: NEString.t;
    person: ModelBuilder.Core.Person.t Entry.Id.t;
  }
  [@@deriving to_biniou, to_yojson]
  let to_proxy (user : t) : proxy =
    {username = user.username; person = user.person}

  let to_biniou = proxy_to_biniou % to_proxy
  let to_yojson = proxy_to_yojson % to_proxy
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
