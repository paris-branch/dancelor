open Nes

(** How to render the order. [Default] prints the tunes as they appear in the
    set. [Unfolded] follows the order, duplicating the tunes if they are to be
    played several times. *)
type order_type = Default | Unfolded
[@@deriving yojson]

(** The size of the paper to use. [A] allows to select the ISO 216 “A” format.
    [Custom] allows to give a custom value. The default is [A 4]. *)
type paper_size =
  | A of int
  | Custom of float * float * string (** width, height and unit *)
[@@deriving yojson]

module Self = struct
  type t =
    { forced_pages  : int    option [@default None] [@key "forced-pages"] ;
      show_deviser  : bool   option [@default None] [@key "show-deviser"] ;
      show_order    : bool   option [@default None] [@key "show-order"] ;
      order_type    : order_type option [@default None] [@key "order-type"] ;
      display_name  : string option [@default None] [@key "display-name"] ;
      paper_size    : paper_size option [@default None] [@key "paper-size"] ;

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

let forced_pages p = Option.get p.forced_pages
let for_dance    p = p.for_dance
let display_name p = p.display_name
let show_deviser p = Option.get p.show_deviser
let show_order   p = Option.get p.show_order
let order_type   p = p.order_type
let paper_size   p = Option.get p.paper_size
let paper_size_opt p = p.paper_size

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
  paper_size = Some (A 4) ;

  every_version = VersionParameters.default ;
}

let compose first second =
  { forced_pages  = Option.(choose ~tie:second) first.forced_pages second.forced_pages ;
    for_dance     = Option.(choose ~tie:fail)   first.for_dance    second.for_dance ;
    display_name  = Option.(choose ~tie:second) first.display_name second.display_name ;
    show_deviser  = Option.(choose ~tie:second) first.show_deviser second.show_deviser ;
    show_order    = Option.(choose ~tie:second) first.show_order   second.show_order ;
    order_type    = Option.(choose ~tie:second) first.order_type   second.order_type ;
    paper_size    = Option.(choose ~tie:second) first.paper_size   second.paper_size ;

    every_version = VersionParameters.compose first.every_version second.every_version }

let fill = compose default
