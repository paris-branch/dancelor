(** {1 Book Parameters}

    This module defines parameters that make sense at the level of a book. This
    includes set parameters (which include version parameters) as well. *)

open Nes

type where =
  Beginning | End | Nowhere
[@@deriving yojson]

(* FIXME: There are parameters that are really version-, set- or book-specific,
   but there are others (such as [instruments], [paper_size], etc.) that apply
   to all. I think we should extract those in a separate type, for instance
   [GlobalParameters] or [RenderingParameters] or something. *)

module Self = struct
  type t = {
    front_page: bool option; [@default None] [@key "front-page"]
    table_of_contents: where option; [@default None] [@key "table-of-contents"]
    two_sided: bool option; [@default None] [@key "two-sided"]
    running_header: bool option; [@default None] [@key "running-header"]
    running_footer: bool option; [@default None] [@key "running-footer"]
    every_set: SetParameters.t; [@default SetParameters.none] [@key "every-set"]
  }
  [@@deriving make, yojson]
end
include Self

(* FIXME: see remark in VersionParameters *)
let make ?front_page ?table_of_contents ?two_sided ?every_set ?running_header () =
  make ~front_page ~table_of_contents ~two_sided ?every_set ~running_header ()

(** {2 Getters} *)

let front_page p = p.front_page
let table_of_contents p = p.table_of_contents
let two_sided p = p.two_sided
let running_header p = p.running_header
let running_footer p = p.running_footer
let every_set p = p.every_set
let instruments = SetParameters.instruments % every_set

(** {2 Defaults} *)

let none = `Assoc [] |> of_yojson |> Result.get_ok

let front_page' = Option.value ~default: false % front_page
let table_of_contents' = Option.value ~default: Nowhere % table_of_contents
let two_sided' = Option.value ~default: false % two_sided
let running_header' = Option.value ~default: true % running_header
let running_footer' = Option.value ~default: true % running_footer
let instruments' = Option.value ~default: "" % instruments

(** {2 Composition} *)

let compose first second = {
  front_page = Option.(choose ~tie: second) first.front_page second.front_page;
  table_of_contents = Option.(choose ~tie: second) first.table_of_contents second.table_of_contents;
  two_sided = Option.(choose ~tie: second) first.two_sided second.two_sided;
  running_header = Option.(choose ~tie: second) first.running_header second.running_header;
  running_footer = Option.(choose ~tie: second) first.running_footer second.running_footer;
  every_set = SetParameters.compose first.every_set second.every_set
}
