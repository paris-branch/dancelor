(** {1 Set Parameters}

    This module defines parameters that make sense at the level of a set. This
    includes version parameters as well. *)

open Ppx_yojson_conv_lib.Yojson_conv
open Nes

(** How to render the order. [Default] prints the tunes as they appear in the
    set. [Unfolded] follows the order, duplicating the tunes if they are to be
    played several times. *)
type order_type =
  Default | Unfolded
[@@deriving eq, show {with_path = false}, yojson]

module Self = struct
  type t = {
    forced_pages: int option; [@default None] [@key "forced-pages"]
    show_deviser: bool option; [@default None] [@key "show-deviser"]
    show_order: bool option; [@default None] [@key "show-order"]
    order_type: order_type option; [@default None] [@key "order-type"]
    display_name: string option; [@default None] [@key "display-name"]
    for_dance: Dance.t Slug.t option; [@default None] [@key "for-dance"]
    every_version: VersionParameters.t [@default VersionParameters.none] [@key "every-version"]
  }
  [@@deriving eq, make, show {with_path = false}, yojson, fields]
end
include Self

(* FIXME: see remark in VersionParameters *)
let make ?forced_pages ?show_deviser ?show_order ?display_name ?for_dance ?every_version () =
  make ~forced_pages ~show_deviser ~show_order ~display_name ~for_dance ?every_version ()

(** {2 Getters} *)

let instruments = VersionParameters.instruments % every_version

(** {2 Defaults}

    Getters that end with a quote are getters that have a default value. *)

let none = t_of_yojson @@ `Assoc []

let forced_pages' = Option.value ~default: 0 % forced_pages
let show_deviser' = Option.value ~default: true % show_deviser
let show_order' = Option.value ~default: true % show_order
let order_type' = Option.value ~default: Default % order_type
let display_name' ~default = Option.value ~default % display_name

(** {2 Setters} *)

let set_show_order show_order p = {p with show_order = Some show_order}

(** {2 Composition} *)

let compose first second = {
  forced_pages = Option.(choose ~tie: second) first.forced_pages second.forced_pages;
  for_dance = Option.(choose ~tie: fail) first.for_dance second.for_dance;
  display_name = Option.(choose ~tie: second) first.display_name second.display_name;
  show_deviser = Option.(choose ~tie: second) first.show_deviser second.show_deviser;
  show_order = Option.(choose ~tie: second) first.show_order second.show_order;
  order_type = Option.(choose ~tie: second) first.order_type second.order_type;
  every_version = VersionParameters.compose first.every_version second.every_version
}
