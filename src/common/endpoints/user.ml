open Nes
open Madge
open Model_builder.Core
module Filter = Filter_builder.Core

type (_, _, _) t =
  | Get : ((User.t Entry.Id.t -> 'w), 'w, User.entry) t
  | Status : ('w, 'w, User.entry option) t
  | Sign_in : ((Username.t -> User.Password_clear.t -> bool -> 'w), 'w, User.entry option) t
  | Sign_out : ('w, 'w, unit) t
  | Create : ((User.t -> 'w), 'w, User.entry * User.Password_reset_token_clear.t) t
  | Prepare_reset_password : ((Username.t -> 'w), 'w, User.Password_reset_token_clear.t) t
  | Reset_password : ((Username.t -> User.Password_reset_token_clear.t -> User.Password_clear.t -> 'w), 'w, unit) t
  | Search : ((Slice.t -> (User.t, Formula_user.t) Formula_entry.public -> 'w), 'w, (int * (User.t, Entry.Access.public) Entry.t list)) t
  | Set_omniscience : ((bool -> 'w), 'w, unit) t
[@@deriving madge_wrapped_endpoints]

let route : type a w r. (a, w, r) t -> (a, w, r) route =
  let open Route in
  function
    | Get -> variable (module Entry.Id.S(User)) @@ get (module Entry.JPublic(User))
    | Status -> literal "status" @@ post (module JOption(Entry.JPublic(User)))
    | Sign_in -> literal "sign-in" @@ body "username" (module Username) @@ body "password" (module User.Password_clear) @@ body "remember-me" (module JBool) @@ post (module JOption(Entry.JPublic(User)))
    | Sign_out -> literal "sign-out" @@ post (module JUnit)
    | Create -> literal "create" @@ body "user" (module User) @@ post (module JPair(Entry.JPublic(User))(User.Password_reset_token_clear))
    | Prepare_reset_password -> literal "prepare-reset-password" @@ body "username" (module Username) @@ post (module User.Password_reset_token_clear)
    | Reset_password -> literal "reset-password" @@ body "username" (module Username) @@ body "token" (module User.Password_reset_token_clear) @@ body "password" (module User.Password_clear) @@ post (module JUnit)
    | Search -> query "slice" (module Slice) @@ query "filter" (module Formula_entry.JPublic(User)(Formula_user)) @@ get (module JPair(JInt)(JList(Entry.JPublic(User))))
    | Set_omniscience -> literal "set-omniscience" @@ body "value" (module JBool) @@ put (module JUnit)
