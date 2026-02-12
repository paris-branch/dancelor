open Nes
open Madge
open Model_builder.Core
module Filter = Filter_builder.Core

(* NOTE: The user model contains passwords and other secret tokens. Even though
   they are heavily hashed, we shouldn't be sending them, so we redact them. *)
module User = struct
  type t = User.t
  [@@deriving of_yojson]

  type proxy = {
    username: NEString.t;
    role: User.role;
  }
  [@@deriving to_yojson]

  let to_proxy user =
    (* NOTE: To be sure to not miss the introduction of a future field, we match
       explicitly on all the field names; do not use [; _]! *)
    let {
      username;
      role;
      password = _;
      password_reset_token = _;
      remember_me_tokens = _;
    }
        : t
      =
      user
    in
      ({username; role}: proxy)

  let to_yojson = proxy_to_yojson % to_proxy
end

type (_, _, _) t =
  | Get : ((User.t Entry.Id.t -> 'w), 'w, Model_builder.Core.User.entry) t
  | Status : ('w, 'w, Model_builder.Core.User.entry option) t
  | Sign_in : ((string -> string -> bool -> 'w), 'w, Model_builder.Core.User.entry option) t
  | Sign_out : ('w, 'w, unit) t
  | Create : ((User.t -> 'w), 'w, Model_builder.Core.User.entry * string) t
  | Prepare_reset_password : ((string -> 'w), 'w, string) t
  | Reset_password : ((string -> string -> string -> 'w), 'w, unit) t
  | Search : ((Slice.t -> Filter.User.t -> 'w), 'w, (int * (User.t, Entry.Access.public) Entry.t list)) t
  | Set_omniscience : ((bool -> 'w), 'w, unit) t
[@@deriving madge_wrapped_endpoints]

let route : type a w r. (a, w, r) t -> (a, w, r) route =
  let open Route in
  function
    | Get -> variable (module Entry.Id.S(User)) @@ get (module Entry.JPublic(User))
    | Status -> literal "status" @@ post (module JOption(Entry.JPublic(User)))
    | Sign_in -> literal "sign-in" @@ body "username" (module JString) @@ body "password" (module JString) @@ body "remember-me" (module JBool) @@ post (module JOption(Entry.JPublic(User)))
    | Sign_out -> literal "sign-out" @@ post (module JUnit)
    | Create -> literal "create" @@ body "user" (module User) @@ post (module JPair(Entry.JPublic(User))(JString))
    | Prepare_reset_password -> literal "prepare-reset-password" @@ body "username" (module JString) @@ post (module JString)
    | Reset_password -> literal "reset-password" @@ body "username" (module JString) @@ body "token" (module JString) @@ body "password" (module JString) @@ post (module JUnit)
    | Search -> query "slice" (module Slice) @@ query "filter" (module Filter.User) @@ get (module JPair(JInt)(JList(Entry.JPublic(User))))
    | Set_omniscience -> literal "set-omniscience" @@ body "value" (module JBool) @@ put (module JUnit)
