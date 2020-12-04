open Nes

type where = Beginning | End | Nowhere
[@@deriving yojson]

module Self = struct
  type t = {
    instruments       : string option [@default None] ;
    front_page        : bool   option [@default None] [@key "front-page"] ;
    table_of_contents : where  option [@default None] [@key "table-of-contents"] ;
    two_sided         : bool   option [@default None] [@key "two-sided"] ;
    running_header    : bool   option [@default None] [@key "running-header"] ;

    every_set : SetParameters.t [@default SetParameters.none] [@key "every-set"] ;
  }
  [@@deriving make, yojson]

  let _key = "book-parameters"
end
include Self

(* FIXME: see remark in VersionParameters *)
let make ?instruments ?front_page ?table_of_contents ?two_sided ?every_set () =
  make ~instruments ~front_page ~table_of_contents ~two_sided ?every_set ()

let make_instrument pitch =
  make
    ~instruments:(Music.pitch_to_pretty_string pitch ^ " instruments")
    ~every_set:(SetParameters.make_instrument pitch)
    ()

let none = `Assoc [] |> of_yojson |> Result.get_ok

let instruments       p = p.instruments
let every_set         p = p.every_set
let front_page        p = p.front_page
let table_of_contents p = p.table_of_contents
let two_sided         p = p.two_sided
let running_header    p = p.running_header

let compose first second =
  { instruments       = Option.choose_strict first.instruments       second.instruments ;
    front_page        = Option.choose_strict first.front_page        second.front_page ;
    table_of_contents = Option.choose_strict first.table_of_contents second.table_of_contents ;
    two_sided         = Option.choose_strict first.two_sided         second.two_sided ;
    running_header    = Option.choose_strict first.running_header    second.running_header ;

    every_set = SetParameters.compose first.every_set second.every_set }
