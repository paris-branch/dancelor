open NesPervasives

type t = {
  salt: string;
  hash: string
}
[@@deriving show, eq, yojson]

let salt () =
  Sha512.(to_hex % string) @@
  Printf.sprintf "%Lx" @@
  Random.int64_in_range ~min: Int64.min_int ~max: Int64.max_int

let hash ~salt clear =
  Sha512.(to_hex % string) (salt ^ clear)

let make ~clear =
  let salt = salt () in
    {salt; hash = hash clear ~salt}

let is ~clear password =
  hash clear ~salt: password.salt = password.hash
