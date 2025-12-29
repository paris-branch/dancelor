open Nes

type t = {
  username: NEString.t;
  password: HashedSecret.t option; [@default None]
  password_reset_token: (HashedSecret.t * Datetime.t) option; [@default None] [@key "password-reset-token"]
  remember_me_tokens: (HashedSecret.t * Datetime.t) String.Map.t; [@default String.Map.empty] [@key "remember-me-token"]
}
[@@deriving eq, make, yojson, fields, show {with_path = false}]

let make ~username ?password ?password_reset_token ?remember_me_tokens () =
  let username = NEString.map_exn (String.remove_duplicates ~char: ' ') username in
  make ~username ~password ~password_reset_token ?remember_me_tokens ()

let update ?username ?password ?password_reset_token ?remember_me_tokens user =
  lwt {
    username = Option.value username ~default: Fun.id user.username;
    password = Option.value password ~default: Fun.id user.password;
    password_reset_token = Option.value password_reset_token ~default: Fun.id user.password_reset_token;
    remember_me_tokens = Option.value remember_me_tokens ~default: Fun.id user.remember_me_tokens;
  }
