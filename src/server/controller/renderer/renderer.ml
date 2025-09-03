(** {1 Renderer} *)

open NesUnix

(** {2 Models for the rendering} *)

type tune = {
  name: string;
  composer: string;
  content: string;
  first_bar: int;
}
[@@deriving yojson]

type part = {
  name: string;
}
[@@deriving yojson]

type tune_with_slug = {
  slug: string;
  tune: tune;
}
[@@deriving yojson]

type set = {
  name: string;
  conceptor: string;
  kind: string;
  contents: tune_with_slug list;
}
[@@deriving yojson]

type page =
  | Part of part
  | Set of set
[@@deriving variants]

let page_to_yojson = function
  | Part part -> `Assoc [("part", part_to_yojson part)]
  | Set set -> `Assoc [("set", set_to_yojson set)]

let page_of_yojson = function
  | `Assoc [("part", json)] -> Result.map part @@ part_of_yojson json
  | `Assoc [("set", json)] -> Result.map set @@ set_of_yojson json
  | _ -> Error "page_of_yojson"

type book = {
  title: string;
  editor: string;
  contents: page list;
  simple: bool;
}
[@@deriving yojson]

(** {2 Nix integration} *)
(* TODO: This would probably be much faster if we used Nix as a library. *)

let escape_double_quotes string =
  let buf = Buffer.create (String.length string) in
  Buffer.add_char buf '"';
  String.iter
    (function
      | '"' -> Buffer.add_string buf "\\\""
      | '\\' -> Buffer.add_string buf "\\\\"
      | c -> Buffer.add_char buf c
    )
    string;
  Buffer.add_char buf '"';
  Buffer.contents buf

type call_nix_config = {
  share: string;
  nixpkgs: string;
  nixpkgs2211: string;
}

let call_nix fun_ json =
  Job.make
    (
      spf
        "(import %s/renderer/renderer.nix { %s%s}).%s (builtins.fromJSON %s)"
        (if (!Config.share).[0] = '/' then !Config.share else "./" ^ !Config.share)
        (if !Config.nixpkgs = "" then "" else "nixpkgs = " ^ escape_double_quotes !Config.nixpkgs ^ "; ")
        (if !Config.nixpkgs2211 = "" then "" else "nixpkgs2211 = " ^ escape_double_quotes !Config.nixpkgs2211 ^ "; ")
        fun_
        (escape_double_quotes (Yojson.Safe.to_string json))
    )

(** {2 API} *)

type tune_svg_arg = {
  slug: string;
  tune: tune;
  stylesheet: string;
}
[@@deriving yojson]

let make_tune_svg = call_nix "makeTuneSvg" % tune_svg_arg_to_yojson

type tune_ogg_arg = {
  slug: string;
  tune: tune;
  tempo_unit: string;
  tempo_value: int;
  chords_kind: string;
}
[@@deriving yojson]

let make_tune_ogg = call_nix "makeTuneOgg" % tune_ogg_arg_to_yojson

type pdf_metadata = {
  title: string;
  authors: string list;
  subjects: string list;
  creator: string; (* FIXME *)
}
[@@deriving yojson]

type book_pdf_arg = {
  slug: string;
  book: book;
  specificity: string;
  headers: bool;
  pdf_metadata: pdf_metadata;
}
[@@deriving yojson]

let make_book_pdf = call_nix "makeBookPdf" % book_pdf_arg_to_yojson
