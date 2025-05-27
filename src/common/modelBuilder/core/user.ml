open Nes

let _key = "user"

type t = {
  display_name: string; [@key "display-name"]
  person: Person.t Slug.t;
  password: HashedSecret.t option; [@default None]
  password_reset_token: (HashedSecret.t * Datetime.t) option; [@default None] [@key "password-reset-token"]
  remember_me_tokens: (HashedSecret.t * Datetime.t) String.Map.t; [@default String.Map.empty] [@key "remember-me-token"]
}
[@@deriving make, yojson, fields]

let make ~display_name ~person ?password ?password_reset_token ?remember_me_tokens () =
  make ~display_name ~person ~password ~password_reset_token ?remember_me_tokens ()

let update ?display_name ?person ?password ?password_reset_token ?remember_me_tokens user =
  let%lwt person = Option.value person ~default: lwt user.person in
  lwt {
    display_name = Option.value display_name ~default: Fun.id user.display_name;
    person;
    password = Option.value password ~default: Fun.id user.password;
    password_reset_token = Option.value password_reset_token ~default: Fun.id user.password_reset_token;
    remember_me_tokens = Option.value remember_me_tokens ~default: Fun.id user.remember_me_tokens;
  }

let display_name' = display_name % Entry.value
let password' = password % Entry.value
let password_reset_token' = password_reset_token % Entry.value
let remember_me_tokens' = remember_me_tokens % Entry.value

let admin user = Entry.slug_as_string user = "niols"
