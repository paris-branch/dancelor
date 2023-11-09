open Nes

type where = Beginning | End | Nowhere
[@@deriving yojson]

(* FIXME: There are parameters that are really version-, set- or book-specific,
   but there are others (such as [instruments], [paper_size], etc.) that apply
   to all. I think we should extract those in a separate type, for instance
   [GlobalParameters] or [RenderingParameters] or something. *)

module Self = struct
  type t = {
    front_page        : bool   option [@default None] [@key "front-page"] ;
    table_of_contents : where  option [@default None] [@key "table-of-contents"] ;
    two_sided         : bool   option [@default None] [@key "two-sided"] ;
    running_header    : bool   option [@default None] [@key "running-header"] ;
    running_footer    : bool   option [@default None] [@key "running-footer"] ;
    paper_size        : SetParameters.paper_size option [@default None] [@key "paper-size"] ;

    every_set : SetParameters.t [@default SetParameters.none] [@key "every-set"] ;
  }
  [@@deriving make, yojson]

  let _key = "book-parameters"
end
include Self

(* FIXME: see remark in VersionParameters *)
let make ?front_page ?table_of_contents ?two_sided ?every_set ?running_header ?paper_size () =
  make ~front_page ~table_of_contents ~two_sided ?every_set ~running_header ~paper_size ()

let front_page        p = Option.unwrap p.front_page
let table_of_contents p = Option.unwrap p.table_of_contents
let two_sided         p = Option.unwrap p.two_sided
let running_header    p = Option.unwrap p.running_header
let running_footer    p = Option.unwrap p.running_footer
let paper_size        p = Option.unwrap p.paper_size

let every_set         p = p.every_set
let instruments = SetParameters.instruments % every_set

let none = `Assoc [] |> of_yojson |> Result.get_ok

let default = {
  front_page = Some false ;
  table_of_contents = Some Nowhere ;
  two_sided = Some false ;
  running_header = Some true ;
  running_footer = Some true ;
  paper_size = SetParameters.default.paper_size ;

  every_set = SetParameters.default ;
}

let compose first second =
  { front_page        = Option.(choose ~tie:second) first.front_page        second.front_page ;
    table_of_contents = Option.(choose ~tie:second) first.table_of_contents second.table_of_contents ;
    two_sided         = Option.(choose ~tie:second) first.two_sided         second.two_sided ;
    running_header    = Option.(choose ~tie:second) first.running_header    second.running_header ;
    running_footer    = Option.(choose ~tie:second) first.running_footer    second.running_footer ;
    paper_size        = Option.(choose ~tie:second) first.paper_size        second.paper_size ;

    every_set = SetParameters.compose first.every_set second.every_set }

let fill = compose default
