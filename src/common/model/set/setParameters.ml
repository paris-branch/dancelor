open Nes

(** How to render the order. [Default] prints the tunes as they appear in the
    set. [Unfolded] follows the order, duplicating the tunes if they are to be
    played several times. *)
type order_type = Default | Unfolded
[@@deriving yojson]

module Self = struct
  type t =
    { forced_pages  : int    option [@default None] [@key "forced-pages"] ;
      show_deviser  : bool   option [@default None] [@key "show-deviser"] ;
      show_order    : bool   option [@default None] [@key "show-order"] ;
      order_type    : order_type option [@default None] [@key "order-type"] ;
      display_name  : string option [@default None] [@key "display-name"] ;

      for_dance     : DanceCore.t Slug.t option [@default None] [@key "for-dance"] ;

      every_version : VersionParameters.t [@default VersionParameters.none] [@key "every-version"] }
  [@@deriving make, yojson]

  let _key = "set-parameters"
end
include Self

(* FIXME: see remark in VersionParameters *)
let make
    ?forced_pages ?show_deviser ?show_order
    ?display_name ?for_dance ?every_version ()
  =
  make
    ~forced_pages ~show_deviser ~show_order
    ~display_name ~for_dance ?every_version ()

let forced_pages p = Option.unwrap p.forced_pages
let for_dance    p = p.for_dance
let display_name p = p.display_name
let show_deviser p = Option.unwrap p.show_deviser
let show_order   p = Option.unwrap p.show_order
let order_type   p = p.order_type

let every_version p = p.every_version
let instruments = VersionParameters.instruments % every_version

let set_show_order show_order p =
  { p with show_order = Some show_order }

let none = `Assoc [] |> of_yojson |> Result.get_ok

let default = {
  forced_pages = Some 0 ;
  for_dance = None ;
  display_name = None ;
  show_deviser = Some true ;
  show_order = Some true ;
  order_type = Some Default ;

  every_version = VersionParameters.default ;
}

let compose first second =
  { forced_pages  = Option.(choose ~tie:second) first.forced_pages second.forced_pages ;
    for_dance     = Option.(choose ~tie:fail)   first.for_dance    second.for_dance ;
    display_name  = Option.(choose ~tie:second) first.display_name second.display_name ;
    show_deviser  = Option.(choose ~tie:second) first.show_deviser second.show_deviser ;
    show_order    = Option.(choose ~tie:second) first.show_order   second.show_order ;
    order_type    = Option.(choose ~tie:second) first.order_type   second.order_type ;

    every_version = VersionParameters.compose first.every_version second.every_version }

let fill = compose default
