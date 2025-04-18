type t = [%import: Argon2.encoded]
[@@deriving show, eq, yojson]

let make ~clear =
  (* Default value taken from the examples of [ocaml-argon2], but also checked
     to be equal or stronger to the ones of the [argon2] Rust crate. *)
  let hash_len = 32 in
  let salt_len = 16 in
  let t_cost = 2 in
  let m_cost = 65536 in
  let parallelism = 1 in
  let kind = Argon2.ID in
  let version = Argon2.VERSION_13 in
  let encoded_len = Argon2.encoded_len ~t_cost ~m_cost ~parallelism ~salt_len ~hash_len ~kind in
  (* Let's go. *)
  let salt = String.init salt_len (fun _ -> Char.chr (Random.int 256)) in
  snd @@ Result.get_ok @@ Argon2.hash ~t_cost ~m_cost ~parallelism ~kind ~hash_len ~encoded_len ~version ~salt ~pwd: clear

let is ~clear password =
  (* Must match kind in {!make}. *)
  let kind = Argon2.ID in
  (* Let's go. *)
  Result.get_ok @@ Argon2.verify ~kind ~encoded: password ~pwd: clear
