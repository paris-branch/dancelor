(* This module only exists to expose the type of hashed secrets everywhere, but
   the interesting stuff lives in [NesHashedSecretUnix]. *)

open Ppx_yojson_conv_lib.Yojson_conv

type t = string
[@@deriving show, eq, yojson]

let unsafe_of_string = Fun.id
let unsafe_to_string = Fun.id
