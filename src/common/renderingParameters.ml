(** {1 Rendering parameters} *)

open Ppx_yojson_conv_lib.Yojson_conv
open Nes

(** The size of the paper to use. [A] allows to select the ISO 216 “A” format.
    [Custom] allows to give a custom value. The default is [A 4]. *)
type paper_size =
  | A of int
  | Custom of float * float * string (** width, height and unit *)
[@@deriving eq, show {with_path = false}, yojson]

type pdf_metadata = {
  title: string; [@default ""]
  subtitle: string; [@default ""]
  composers: string list; [@default []] (* FIXME: difference between “author” and “composer”? *)
  subjects: string list; [@default []] (** to be used for eg. kinds *)
}
[@@deriving fields, show, eq, yojson]

let no_pdf_metadata = pdf_metadata_of_yojson @@ `Assoc []

let update_pdf_metadata ?title ?subtitle ?composers ?subjects pdf_metadata = {
  title = Option.value title ~default: Fun.id pdf_metadata.title;
  subtitle = Option.value subtitle ~default: Fun.id pdf_metadata.subtitle;
  composers = Option.value composers ~default: Fun.id pdf_metadata.composers;
  subjects = Option.value subjects ~default: Fun.id pdf_metadata.subjects;
}

type t = {
  paper_size: paper_size option; [@default None] [@key "paper-size"]
  pdf_metadata: pdf_metadata; [@default no_pdf_metadata]
}
[@@deriving fields, yojson, eq, show]

let none = t_of_yojson @@ `Assoc []

let update ?paper_size ?pdf_metadata params = {
  paper_size = Option.value paper_size ~default: Fun.id params.paper_size;
  pdf_metadata = Option.value pdf_metadata ~default: Fun.id params.pdf_metadata;
}

let paper_size' = Option.value ~default: (A 4) % paper_size
