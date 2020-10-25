
type where = Beginning | End | Nowhere
[@@deriving yojson]

module Self = struct
  type t = {
    instruments : string Parameter.t                          [@default Parameter.Undefined] ;
    front_page : bool Parameter.t         [@key "front-page"] [@default Parameter.Undefined] ;
    table_of_contents : where Parameter.t [@key "table-of-contents"] [@default Parameter.Undefined] ;
    two_sided : bool Parameter.t          [@key "two-sided"]  [@default Parameter.Undefined] ;
    every_set : SetParameters.t           [@key "every-set"]  [@default SetParameters.none] ;
  }
  [@@deriving make, yojson]

  let _key = "program-parameters"
end
include Self

let make ?instruments ?front_page ?table_of_contents ?two_sided ?every_set () =
  let instruments = Option.map Parameter.defined instruments in
  let front_page = Option.map Parameter.defined front_page in
  let table_of_contents = Option.map Parameter.defined table_of_contents in
  let two_sided = Option.map Parameter.defined two_sided in
  make ?instruments ?front_page ?table_of_contents ?two_sided ?every_set ()

let make_instrument ?(octave=0) key =
  let instruments = Music.pitch_to_pretty_string key ^ " instruments" in
  make
    ~instruments
    ~every_set:(SetParameters.make_instrument ~octave key)
    ()

let none = `Assoc [] |> of_yojson |> Result.get_ok

let instruments p = p.instruments
let every_set p = p.every_set
let front_page p = p.front_page
let table_of_contents p = p.table_of_contents
let two_sided p = p.two_sided

let compose ~parent parameters =
  { instruments = Parameter.compose parent.instruments parameters.instruments ;
    front_page = Parameter.compose parent.front_page parameters.front_page ;
    table_of_contents = Parameter.compose parent.table_of_contents parameters.table_of_contents ;
    two_sided = Parameter.compose parent.two_sided parameters.two_sided ;
    every_set = SetParameters.compose ~parent:parent.every_set parameters.every_set }
