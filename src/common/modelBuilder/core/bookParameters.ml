(** {1 Book Parameters}

    This module defines parameters that make sense at the level of a book. This
    includes set parameters (which include version parameters) as well. *)

open Nes

module Self = struct
  type t = {
    simple: bool option; [@default None]
    every_set: SetParameters.t; [@default SetParameters.none] [@key "every-set"]
  }
  [@@deriving make, yojson, fields]
end
include Self

(* FIXME: see remark in VersionParameters *)
let make ?simple ?every_set () =
  make ~simple ?every_set ()

(** {2 Defaults} *)

let none = `Assoc [] |> of_yojson |> Result.get_ok

(** {2 Composition} *)

let compose first second = {
  simple = Option.(choose ~tie: second) first.simple second.simple;
  every_set = SetParameters.compose first.every_set second.every_set
}
