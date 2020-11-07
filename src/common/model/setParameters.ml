open Nes

module Self = struct
  type t =
    { instruments   : string option       [@default None] ;
      forced_pages  : int    option       [@default None] [@key "forced-pages"] ;

      every_version : VersionParameters.t [@default VersionParameters.none ] [@key "every-version"] }
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

let none = `Assoc [] |> of_yojson |> Result.get_ok

let instruments   p = p.instruments
let forced_pages  p = p.forced_pages

let every_version p = p.every_version

let compose first second =
  { instruments   = Option.choose_strict first.instruments  second.instruments ;
    forced_pages  = Option.choose_strict first.forced_pages second.forced_pages ;
    every_version = VersionParameters.compose first.every_version second.every_version }
