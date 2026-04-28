open Nes

module Password_clear = Fresh.Make(String)

module Password_reset_token_clear = struct
  include Fresh.Make(String)
  let make () = inject (uid ())
end

type role =
  | Normal_user
  | Maintainer
  | Administrator of {omniscience: bool}
[@@deriving eq, ord, yojson, variants, show]

type t = {
  username: Username.t;
  role: role; [@default Normal_user]
}
[@@deriving eq, ord, make, yojson, fields, show {with_path = false}]

let update ?username ?role user =
  lwt {
    username = Option.value username ~default: Fun.id user.username;
    role = Option.value role ~default: Fun.id user.role;
  }
