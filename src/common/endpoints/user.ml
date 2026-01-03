open Nes
open Madge
open ModelBuilder.Core
module Filter = FilterBuilder.Core

(* NOTE: The user model contains passwords and other secret tokens. Even though
   they are heavily hashed, we shouldn't be sending them, so we redact them. *)
module User = struct
  type t = User.t
  [@@deriving of_yojson]

  type proxy = {
    username: NEString.t;
  }
  [@@deriving to_yojson]

  let to_proxy user =
    (* NOTE: To be sure to not miss the introduction of a future field, we match
       explicitly on all the field names; do not use [; _]! *)
    let {
      username;
      password = _;
      password_reset_token = _;
      remember_me_tokens = _;
      role = _;
    }
        : t
      =
      user
    in
      ({username}: proxy)

  let to_yojson = proxy_to_yojson % to_proxy
end

type (_, _, _) t =
  | Get : ((User.t Entry.Id.t -> 'w), 'w, ModelBuilder.Core.User.entry) t
  | Status : ('w, 'w, ModelBuilder.Core.User.entry option) t
  | CanCreate : ('w, 'w, bool) t
  | CanAdmin : ('w, 'w, bool) t
  | SignIn : ((string -> string -> bool -> 'w), 'w, ModelBuilder.Core.User.entry option) t
  | SignOut : ('w, 'w, unit) t
  | Create : ((User.t -> 'w), 'w, ModelBuilder.Core.User.entry * string) t
  | ResetPassword : ((string -> string -> string -> 'w), 'w, unit) t
  | Search : ((Slice.t -> Filter.User.t -> 'w), 'w, (int * (User.t, Entry.Access.public) Entry.t list)) t
[@@deriving madge_wrapped_endpoints]

let route : type a w r. (a, w, r) t -> (a, w, r) route =
  let open Route in
  function
    | Get -> variable (module Entry.Id.S(User)) @@ get (module Entry.JPublic(User))
    | Status -> literal "status" @@ post (module JOption(Entry.JPublic(User)))
    | SignIn -> literal "sign-in" @@ body "username" (module JString) @@ body "password" (module JString) @@ body "remember-me" (module JBool) @@ post (module JOption(Entry.JPublic(User)))
    | SignOut -> literal "sign-out" @@ post (module JUnit)
    | Create -> literal "create" @@ body "user" (module User) @@ post (module JPair(Entry.JPublic(User))(JString))
    | ResetPassword -> literal "reset-password" @@ body "username" (module JString) @@ body "token" (module JString) @@ body "password" (module JString) @@ post (module JUnit)
    | CanCreate -> literal "can-create" @@ post (module JBool)
    | CanAdmin -> literal "can-admin" @@ post (module JBool)
    | Search -> query "slice" (module Slice) @@ query "filter" (module Filter.User) @@ get (module JPair(JInt)(JList(Entry.JPublic(User))))
