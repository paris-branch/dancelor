open Nes

let _key = "user"

include Entry.User

type access = Entry.Access.public [@@deriving yojson]
type entry = t Entry.public
[@@deriving eq, show, yojson]

let username' = username % Entry.value_public
let password' = password % Entry.value_public
let password_reset_token' = password_reset_token % Entry.value_public
let remember_me_tokens' = remember_me_tokens % Entry.value_public

let is_maintainer _user = false
let is_administrator user = username' user = NEString.of_string_exn "Niols"
