(** {1 Book Parameters}

    This module defines parameters that make sense at the level of a book. This
    includes set parameters (which include version parameters) as well. *)

open Nes

module Self = struct
  type t = {
    every_set: SetParameters.t; [@default SetParameters.none] [@key "every-set"]
  }
  [@@deriving make, yojson, fields]
end
include Self

(* FIXME: see remark in VersionParameters *)
let make ?every_set () =
  make ?every_set ()

(** {2 Defaults} *)

let none = `Assoc [] |> of_yojson |> Result.get_ok

(** {2 Composition} *)

let compose first second = {
  every_set = SetParameters.compose first.every_set second.every_set
}
