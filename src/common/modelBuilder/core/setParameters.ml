(** {1 Set Parameters}

    This module defines parameters that make sense at the level of a set. This
    includes version parameters as well. *)

open Nes

module Self = struct
  type t = {
    show_deviser: bool option; [@default None] [@key "show-deviser"]
    show_order: bool option; [@default None] [@key "show-order"]
    display_name: NEString.t option; [@default None] [@key "display-name"]
    display_conceptor: string option; [@default None] [@key "display-conceptor"]
    display_kind: Kind.Dance.t option; [@default None] [@key "display-kind"]
    every_version: VersionParameters.t [@default VersionParameters.none] [@key "every-version"]
  }
  [@@deriving eq, make, show {with_path = false}, yojson, fields]
end
include Self

(* FIXME: see remark in VersionParameters *)
let make ?show_deviser ?show_order ?display_name ?display_conceptor ?display_kind ?every_version () =
  make ~show_deviser ~show_order ~display_name ~display_conceptor ~display_kind ?every_version ()

(** {2 Defaults}

    Getters that end with a quote are getters that have a default value. *)

let none = `Assoc [] |> of_yojson |> Result.get_ok

let show_deviser' = Option.value ~default: true % show_deviser
let show_order' = Option.value ~default: true % show_order

(** {2 Setters} *)

let set_show_order show_order p = {p with show_order = Some show_order}

(** {2 Composition} *)

let compose first second = {
  display_name = Option.(choose ~tie: second) first.display_name second.display_name;
  display_conceptor = Option.(choose ~tie: second) first.display_conceptor second.display_conceptor;
  display_kind = Option.(choose ~tie: second) first.display_kind second.display_kind;
  show_deviser = Option.(choose ~tie: second) first.show_deviser second.show_deviser;
  show_order = Option.(choose ~tie: second) first.show_order second.show_order;
  every_version = VersionParameters.compose first.every_version second.every_version
}
