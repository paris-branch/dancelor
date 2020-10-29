
module Self = struct
  type t =
    { instruments : string Parameter.t [@default Parameter.Undefined];
      forced_pages : int Parameter.t [@key "forced-pages"] [@default Parameter.Undefined];
      every_version : VersionParameters.t [@key "every-version"] [@default VersionParameters.none ]}
  [@@deriving make, yojson]

  let _key = "set-parameters"
end
include Self

let make ?instruments ?forced_pages ?every_version () =
  let instruments = Option.map Parameter.defined instruments in
  let forced_pages = Option.map Parameter.defined forced_pages in
  make ?instruments ?forced_pages ?every_version ()

let make_instrument pitch =
  let instruments =
    Music.pitch_to_pretty_string
      ~strict_octave:false
      pitch ^ " instruments"
  in
  make
    ~instruments
    ~every_version:(
      VersionParameters.make
        ~transposition:(Transposition.relative pitch Music.pitch_c)
        ()
    )
    ()

let none = `Assoc [] |> of_yojson |> Result.get_ok

let instruments p = p.instruments
let every_version p = p.every_version
let forced_pages p = p.forced_pages

let compose ~parent parameters =
  { instruments = Parameter.compose parent.instruments parameters.instruments ;
    forced_pages = Parameter.compose parent.forced_pages parameters.forced_pages ;
    every_version = VersionParameters.compose ~parent:parent.every_version parameters.every_version }
