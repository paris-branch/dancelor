open Nes

let _key = "user"

include Entry.User

let username' = username % Entry.value
let password' = password % Entry.value
let password_reset_token' = password_reset_token % Entry.value
let remember_me_tokens' = remember_me_tokens % Entry.value

let admin user = username' user = NEString.of_string_exn "Niols"
