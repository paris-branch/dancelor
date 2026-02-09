(** {1 Renderer} *)

open NesUnix

(** {2 Models for the rendering} *)

type tune = {
  slug: string;
  name: string;
  instructions: string;
  composer: string;
  content: string;
  first_bar: int;
  tempo_unit: string;
  tempo_value: int;
  chords_kind: string;
  show_bar_numbers: bool;
  show_time_signatures: bool;
}
[@@deriving yojson]

type part = {
  name: string;
}
[@@deriving yojson]

type set = {
  slug: string;
  name: string;
  conceptor: string;
  kind: string;
  contents: tune list;
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
  slug: string;
  title: string;
  editor: string;
  contents: page list;
  simple: bool; (* FIXME: should that not go into {!book_pdf_arg} *)
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
}

let call_nix fun_ json =
  let%lwt (fname, ochan) = Lwt_io.open_temp_file () in
  Lwt_io.write ochan (Yojson.Safe.to_string json);%lwt
  Lwt_io.close ochan;%lwt
  lwt @@
    Job.Expr (
      spf
        "(import %s/renderer/renderer.nix { %s}).%s (builtins.fromJSON (builtins.readFile %s))"
        (if (!Config.share).[0] = '/' then !Config.share else "./" ^ !Config.share)
        (if !Config.nixpkgs = "" then "" else "nixpkgs = " ^ escape_double_quotes !Config.nixpkgs ^ "; ")
        fun_
        (escape_double_quotes fname),
      [fname]
    )

(** {2 API} *)

let make_tune_snippets = call_nix "makeTuneSnippets" % tune_to_yojson
let make_tune_svg tune = Pair.snoc "snippet.svg" <$> make_tune_snippets tune
let make_tune_ogg tune = Pair.snoc "snippet.ogg" <$> make_tune_snippets tune

type pdf_metadata = {
  title: string;
  authors: string list;
  subjects: string list;
}
[@@deriving yojson]

type book_pdf_arg = {
  book: book;
  specificity: string;
  headers: bool;
  pdf_metadata: pdf_metadata;
}
[@@deriving yojson]

let make_book_pdf arg =
  Pair.snoc "book.pdf" <$> call_nix "makeBookPdf" (book_pdf_arg_to_yojson arg)
