open Nes

let _key = "user"

type t = {
  person: Person.t Slug.t;
  password: HashedSecret.t option; [@default None]
  password_reset_token: (HashedSecret.t * Datetime.t) option; [@default None] [@key "password-reset-token"]
  remember_me_token: (HashedSecret.t * Datetime.t) option; [@default None] [@key "remember-me-token"]
}
[@@deriving make, yojson, fields]

let make ~person ?password ?password_reset_token ?remember_me_token () =
  make ~person ~password ~password_reset_token ~remember_me_token ()

let person = person % Entry.value
let password = password % Entry.value
let password_reset_token = password_reset_token % Entry.value
let remember_me_token = remember_me_token % Entry.value
