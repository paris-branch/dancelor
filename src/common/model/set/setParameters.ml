open Nes

module Self = struct
  type t =
    { forced_pages  : int    option [@default None] [@key "forced-pages"] ;
      show_deviser  : bool   option [@default None] [@key "show-deviser"] ;
      show_order    : bool   option [@default None] [@key "show-order"] ;
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

  every_version = VersionParameters.default ;
}

let compose first second =
  { forced_pages  = Option.(choose ~tie:second) first.forced_pages second.forced_pages ;
    for_dance     = Option.(choose ~tie:fail)   first.for_dance    second.for_dance ;
    display_name  = Option.(choose ~tie:second) first.display_name second.display_name ;
    show_deviser  = Option.(choose ~tie:second) first.show_deviser second.show_deviser ;
    show_order    = Option.(choose ~tie:second) first.show_order   second.show_order ;

    every_version = VersionParameters.compose first.every_version second.every_version }

let fill = compose default
