
module Self = struct
  type t =
    { instruments : string Parameter.t [@default Parameter.Undefined];
      every_version : VersionParameters.t [@key "every-version"] [@default VersionParameters.none ]}
  [@@deriving make, yojson]

  let _key = "set-parameters"
end
include Self

let make ?instruments ?every_version () =
  let instruments = Option.map Parameter.defined instruments in
  make ?instruments ?every_version ()

let make_instrument ?(octave=0) key =
  let instruments = Music.pitch_to_pretty_string key ^ " instruments" in
  let source =
    Music.pitch_to_string key
    ^ (if octave < 0 then
         String.make (-octave) ','
       else if octave > 0 then
         String.make octave '\''
       else
         "")
  in
  make
    ~instruments
    ~every_version:(
      VersionParameters.make
        ~transposition:(Transposition.relative source "c")
        ()
    )
    ()

let none = `Assoc [] |> of_yojson |> Result.get_ok

let instruments p = p.instruments
let every_version p = p.every_version

let compose ~parent parameters =
  { instruments = Parameter.compose parent.instruments parameters.instruments ;
    every_version = VersionParameters.compose ~parent:parent.every_version parameters.every_version }
