open Nes

type where = Beginning | End | Nowhere
[@@deriving yojson]

module Self = struct
  type t = {
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
let make ?front_page ?table_of_contents ?two_sided ?every_set ?running_header () =
  make ~front_page ~table_of_contents ~two_sided ?every_set ~running_header ()

let front_page        p = Option.unwrap p.front_page
let table_of_contents p = Option.unwrap p.table_of_contents
let two_sided         p = Option.unwrap p.two_sided
let running_header    p = Option.unwrap p.running_header

let every_set         p = p.every_set
let instruments = SetParameters.instruments % every_set

let none = `Assoc [] |> of_yojson |> Result.get_ok

let default = {
  front_page = Some false ;
  table_of_contents = Some Nowhere ;
  two_sided = Some false ;
  running_header = Some false ;

  every_set = SetParameters.default ;
}

let compose first second =
  { front_page        = Option.(choose ~tie:second) first.front_page        second.front_page ;
    table_of_contents = Option.(choose ~tie:second) first.table_of_contents second.table_of_contents ;
    two_sided         = Option.(choose ~tie:second) first.two_sided         second.two_sided ;
    running_header    = Option.(choose ~tie:second) first.running_header    second.running_header ;

    every_set = SetParameters.compose first.every_set second.every_set }

let fill = compose default
