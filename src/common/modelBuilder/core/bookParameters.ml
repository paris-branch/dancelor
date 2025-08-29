(** {1 Book Parameters}

    This module defines parameters that make sense at the level of a book. This
    includes set parameters (which include version parameters) as well. *)

open Nes

module Self = struct
  type t = {
    two_sided: bool option; [@default None] [@key "two-sided"]
    every_set: SetParameters.t; [@default SetParameters.none] [@key "every-set"]
  }
  [@@deriving make, yojson, fields]
end
include Self

(* FIXME: see remark in VersionParameters *)
let make ?two_sided ?every_set () =
  make ~two_sided ?every_set ()

(** {2 Defaults} *)

let none = `Assoc [] |> of_yojson |> Result.get_ok

let two_sided' = Option.value ~default: false % two_sided

(** {2 Composition} *)

let compose first second = {
  two_sided = Option.(choose ~tie: second) first.two_sided second.two_sided;
  every_set = SetParameters.compose first.every_set second.every_set
}
