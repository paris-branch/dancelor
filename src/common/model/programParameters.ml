
module Self = struct
  type t =
    { instruments : string Parameter.t [@default Parameter.Undefined] ;
      every_set : SetParameters.t [@key "every-set"] [@default SetParameters.none] }
  [@@deriving make, yojson]

  let _key = "program-parameters"
end
include Self

let make ?instruments ?every_set () =
  let instruments = Option.map Parameter.defined instruments in
  make ?instruments ?every_set ()

let make_instrument ?(octave=0) key =
  let instruments = Music.pitch_to_pretty_string key ^ " instruments" in
  make
    ~instruments
    ~every_set:(SetParameters.make_instrument ~octave key)
    ()

let none = `Assoc [] |> of_yojson |> Result.get_ok

let instruments p = p.instruments
let every_set p = p.every_set
