open Nes

module Password_reset_token_clear = struct
  include Fresh.Make(String)
  let make () = inject (uid ())
end

(* NOTE: This module should not really live in common, but at the moment we're
   merging database models and API models. When we make a proper distinction,
   this should contain an alias for HashedSecret.make. *)
module Password_reset_token_hashed = Fresh.Make(HashedSecret)

module Remember_me_key = struct
  include Fresh.Make(String)
  module Map = struct
    include Map.Make(struct
      type nonrec t = t
      let compare a b = String.compare (project a) (project b)
    end)

    let to_yojson value_to_yojson map =
      `Assoc (List.map (fun (k, v) -> (project k, value_to_yojson v)) (bindings map))

    let of_yojson value_of_yojson = function
      | `Assoc pairs ->
        List.fold_left
          (fun acc (k, v) ->
            Result.bind acc @@ fun map ->
            Result.map (fun v -> add (inject k) v map) (value_of_yojson v)
          )
          (Ok empty)
          pairs
      | _ -> Error "Expected JSON object"

    let pp value_pp fmt map =
      Format.fprintf fmt "@[<hov 2>{@ ";
      iter
        (fun k v ->
          Format.fprintf fmt "%s: %a;@ " (project k) value_pp v
        )
        map;
      Format.fprintf fmt "}@]"
  end
end

module Remember_me_token_clear = Fresh.Make(String)

(* NOTE: This module should not really live in common, but at the moment we're
   merging database models and API models. When we make a proper distinction,
   this should contain an alias for HashedSecret.make. *)
module Remember_me_token_hashed = Fresh.Make(HashedSecret)

type role =
  | Normal_user
  | Maintainer
  | Administrator of {omniscience: bool}
[@@deriving eq, yojson, variants, show]

type t = {
  username: NEString.t;
  password: HashedSecret.t option; [@default None]
  password_reset_token: (Password_reset_token_hashed.t * Datetime.t) option; [@default None] [@key "password-reset-token"]
  remember_me_tokens: (Remember_me_token_hashed.t * Datetime.t) Remember_me_key.Map.t; [@default Remember_me_key.Map.empty] [@key "remember-me-token"]
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
