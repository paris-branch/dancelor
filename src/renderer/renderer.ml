(** {1 Renderer} *)

open NesUnix

(** {2 Models for the rendering} *)

type tune = {
  name: string;
  composer: string;
  content: string;
}
[@@deriving yojson]

type part = {
  name: string;
}
[@@deriving yojson]

type set = {
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
  title: string;
  editor: string;
  specificity: string;
  contents: page list;
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

let call_nix fun_ json =
  Lwt_io.with_temp_file @@ fun (fname, ochan) ->
  (* or, to keep the file around for debugging, change the previous line for: *)
  (* let%lwt (fname, ochan) = Lwt_io.open_temp_file () in *)
  Lwt_io.write ochan (Yojson.Safe.to_string json);%lwt
  let%lwt output =
    Process.run
      ~on_wrong_status: Logs.Error
      [
        "nix";
        "build";
        "--print-build-logs";
        "--log-format";
        "raw";
        "--json";
        "--no-link";
        "--impure";
        "--expr";
        spf
          "(import ./src/renderer/renderer.nix {}).%s (builtins.fromJSON (builtins.readFile %s))"
          fun_
          (escape_double_quotes fname)
      ]
  in
  match Yojson.Safe.from_string output.stdout with
  (* two cases depending on whether there is `startTime` and `endTime` or not *)
  | `List [`Assoc [_; ("outputs", `Assoc [("out", `String pdf)]); _; _]]
  | `List [`Assoc [_; ("outputs", `Assoc [("out", `String pdf)])]] ->
    lwt pdf
  | _ -> lwt "sldkfj"

(** {2 API} *)

type tune_svg_arg = {
  tune: tune;
  stylesheet: string;
}
[@@deriving yojson]

let make_tune_svg = call_nix "makeTuneSvg" % tune_svg_arg_to_yojson

type tune_ogg_arg = {
  tune: tune;
  tempo_unit: string;
  tempo_value: int;
  chords_kind: string;
}
[@@deriving yojson]

let make_tune_ogg = call_nix "makeTuneOgg" % tune_ogg_arg_to_yojson

type book_pdf_arg = {
  book: book;
  full: bool; (** whether the book should be “full”, eg. with a title page and a table of contents *)
  two_sided: bool; (** whether the document should be two-sided; non-full but two-sided documents look bad *)
}
[@@deriving yojson]

let make_book_pdf book =
  call_nix "makeBookPdf" @@
    book_pdf_arg_to_yojson {
      book;
      full = true;
      two_sided = true;
    }

let make_set_pdf set =
  call_nix "makeBookPdf" @@
    book_pdf_arg_to_yojson {
      book = {title = ""; editor = ""; specificity = ""; contents = [Set set]};
      full = false;
      two_sided = false;
    }
