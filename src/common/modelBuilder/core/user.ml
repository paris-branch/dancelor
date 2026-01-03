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
let role' = role % Entry.value_public

let is_maintainer user = is_maintainer (role user)
let is_maintainer' = is_maintainer % Entry.value_public

let is_administrator user = is_administrator (role user)
let is_administrator' = is_administrator % Entry.value_public

let is_omniscient_administrator user = match role user with Administrator {omniscience} -> omniscience | _ -> false
let is_omniscient_administrator' = is_omniscient_administrator % Entry.value_public
