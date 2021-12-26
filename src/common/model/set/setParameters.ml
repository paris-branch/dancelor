open Nes

module Self = struct
  type t =
    { instruments   : string option [@default None] ;
      forced_pages  : int    option [@default None] [@key "forced-pages"] ;
      show_dance    : DanceCore.t Slug.t option [@default None] [@key "show-dance"] ;
      show_deviser  : bool   option [@default None] [@key "show-deviser"] ;

      every_version : VersionParameters.t [@default VersionParameters.none] [@key "every-version"] }
  [@@deriving make, yojson]

  let _key = "set-parameters"
end
include Self

(* FIXME: see remark in VersionParameters *)
let make ?instruments ?forced_pages ?every_version () =
  make ~instruments ~forced_pages ?every_version ()

let make_instrument pitch =
  make
    ~instruments:(Music.pitch_to_pretty_string pitch ^ " instruments")
    ~every_version:(
      VersionParameters.make
        ~transposition:(Transposition.relative pitch Music.pitch_c)
        ()
    )
    ()

let instruments  p = Option.unwrap p.instruments
let forced_pages p = Option.unwrap p.forced_pages
let show_dance   p = p.show_dance
let show_deviser p = Option.unwrap p.show_deviser

let every_version p = p.every_version

let none = `Assoc [] |> of_yojson |> Result.get_ok

let default = {
  instruments = Some "" ;
  forced_pages = Some 0 ;
  show_dance = None ;
  show_deviser = Some true ;

  every_version = VersionParameters.default ;
}

let compose first second =
  { instruments   = Option.(choose ~tie:second) first.instruments  second.instruments ;
    forced_pages  = Option.(choose ~tie:second) first.forced_pages second.forced_pages ;
    show_dance    = Option.(choose ~tie:fail)   first.show_dance   second.show_dance ;
    show_deviser  = Option.(choose ~tie:second) first.show_deviser second.show_deviser ;

    every_version = VersionParameters.compose first.every_version second.every_version }

let fill = compose default
