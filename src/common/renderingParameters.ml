(** {1 Rendering parameters} *)

open Nes

(** The size of the paper to use. [A] allows to select the ISO 216 “A” format.
    [Custom] allows to give a custom value. The default is [A 4]. *)
type paper_size =
  | A of int
  | Custom of float * float * string (** width, height and unit *)
[@@deriving eq, show {with_path = false}, yojson]

type t = {
  paper_size: paper_size option; [@default None] [@key "paper-size"]
}
[@@deriving fields, yojson, eq, show]

let none = `Assoc [] |> of_yojson |> Result.get_ok

let paper_size' = Option.value ~default: (A 4) % paper_size
