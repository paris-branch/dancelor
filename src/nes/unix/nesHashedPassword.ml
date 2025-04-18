open NesPervasives

type t = [%import: Argon2.encoded]
[@@deriving show, eq, yojson]

(* Default value taken from the examples of [ocaml-argon2], but also checked to
   be equal or stronger to the ones of the [argon2] Rust crate. *)
let hash_len = 32
let salt_len = 16
let t_cost = 2
let m_cost = 65536
let parallelism = 1
let kind = Argon2.ID
let version = Argon2.VERSION_13
let encoded_len = Argon2.encoded_len ~t_cost ~m_cost ~parallelism ~salt_len ~hash_len ~kind

let make ~clear =
  let salt = String.init salt_len (fun _ -> Char.chr (Random.int 256)) in
  snd @@ Result.get_ok @@ Argon2.hash ~t_cost ~m_cost ~parallelism ~kind ~hash_len ~encoded_len ~version ~salt ~pwd: clear

let is ~clear password =
  match Argon2.verify ~kind ~encoded: password ~pwd: clear with
  | Ok _ -> Ok true
  | Error Argon2.ErrorCodes.VERIFY_MISMATCH -> Ok false
  | Error e -> Error e

let of_yojson json =
  (* Sanity check that the “encoded” indeed looks correct. We verify the
     password against a random string, and we care less about the result than
     about whether there is an error on the way. *)
  Result.bind (of_yojson json) @@ fun password ->
  NesResult.map_both
    (is ~clear: "not the password" password)
    ~ok: (Fun.const password)
    ~error: ((^) "Argon2 error: " % Argon2.ErrorCodes.message)

let is ~clear password = Result.get_ok @@ is ~clear password
