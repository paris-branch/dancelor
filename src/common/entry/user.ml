open Nes

module Password_reset_token_clear = struct
  include Fresh.Make(String)
  let make () = inject (uid ())
end

(* NOTE: This module should not really live in common, but at the moment we're
   merging database models and API models. When we make a proper distinction,
   this should contain an alias for HashedSecret.make. *)
module Password_reset_token_hashed = Fresh.Make(HashedSecret)

type role =
  | Normal_user
  | Maintainer
  | Administrator of {omniscience: bool}
[@@deriving eq, yojson, variants, show]

type t = {
  username: NEString.t;
  password: HashedSecret.t option; [@default None]
  password_reset_token: (Password_reset_token_hashed.t * Datetime.t) option; [@default None] [@key "password-reset-token"]
  remember_me_tokens: (HashedSecret.t * Datetime.t) String.Map.t; [@default String.Map.empty] [@key "remember-me-token"]
  role: role; [@default Normal_user]
}
[@@deriving eq, make, yojson, fields, show {with_path = false}]

let make ~username ?password ?password_reset_token ?remember_me_tokens ?role () =
  let username = NEString.map_exn (String.remove_duplicates ~char: ' ') username in
  make ~username ~password ~password_reset_token ?remember_me_tokens ?role ()

let update ?username ?password ?password_reset_token ?remember_me_tokens ?role user =
  lwt {
    username = Option.value username ~default: Fun.id user.username;
    password = Option.value password ~default: Fun.id user.password;
    password_reset_token = Option.value password_reset_token ~default: Fun.id user.password_reset_token;
    remember_me_tokens = Option.value remember_me_tokens ~default: Fun.id user.remember_me_tokens;
    role = Option.value role ~default: Fun.id user.role;
  }
