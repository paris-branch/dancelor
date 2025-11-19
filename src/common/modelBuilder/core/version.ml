open Nes

module Content = struct
  module Part_name = struct
    type t = int (* invariant: ∈ [0; 25] *)
    [@@deriving eq, yojson, show {with_path = false}]

    let of_char c =
      let c = Char.code c in
      if c < 65 || c > 90 then None else Some (c - 65)

    let of_char_exn c =
      match of_char c with
      | Some c -> c
      | None -> failwith "Version.Content.Part_name.of_char_exn: not a letter between A and Z"

    let of_nestring s =
      let s = NEString.to_string s in
      if String.length s = 1 then of_char s.[0] else None

    let to_char c = Char.chr (c + 65)
  end

  type structure = Part_name.t NEList.t
  [@@deriving eq, show {with_path = false}]

  let structure_to_string = NEString.of_string_exn % String.of_seq % Seq.map Part_name.to_char % List.to_seq % NEList.to_list
  let structure_of_string = Option.join % Option.map NEList.of_list % Monadise.Option.monadise_1_1 List.map Part_name.of_char % List.of_seq % String.to_seq % NEString.to_string

  let structure_to_yojson s = `String (NEString.to_string @@ structure_to_string s)
  let structure_of_yojson = function
    | `String s -> Option.to_result ~none: "not a valid structure" (Option.bind (NEString.of_string s) structure_of_string)
    | _ -> Error "not a string"

  type part = {
    melody: string;
    chords: string;
  }
  [@@deriving eq, yojson, show {with_path = false}, fields]

  let lilypond_from_parts kind key parts =
    let time =
      match kind with
      | Kind.Base.Reel -> "2/2"
      | Jig -> "6/8"
      | Strathspey -> "4/4"
      | Waltz -> "3/4"
      | Polka -> "2/2"
    in
    let key =
      (Music.Note.to_lilypond_string @@ Music.Pitch.note @@ Music.Key.pitch key) ^
      " " ^ (Music.Mode.to_lilypond_string key.mode)
    in
    let parts = NEList.to_list parts in
    let melody =
      String.concat " \\section\\break " (
        List.mapi
          (fun part_name part ->
            spf "\\mark\\markup\\box{%c} %s" (Part_name.to_char part_name) part.melody
          )
          parts
      ) ^
        "\\bar \"|.\""
    in
    let chords = String.concat " " (List.map (fun part -> part.chords) parts) in
    spf
      "<< \\new Voice {\\clef treble \\time %s \\key %s {%s}}\\new ChordNames {\\chordmode {%s}}>>"
      time
      key
      melody
      chords

  type destructured = {
    parts: part NEList.t;
    transitions: (Part_name.t option * Part_name.t option * part) list;
    default_structure: structure;
  }
  [@@deriving eq, yojson, show {with_path = false}]

  type t =
    | Monolithic of {lilypond: string; bars: int; structure: structure}
    | Destructured of destructured
  [@@deriving eq, yojson, show {with_path = false}, variants]

  let lilypond kind key = function
    | Monolithic {lilypond; _} -> lilypond
    | Destructured {parts; _} -> lilypond_from_parts kind key parts

  let erase_lilypond = function
    | Monolithic {bars; structure; _} -> Monolithic {bars; structure; lilypond = ""}
    | Destructured {default_structure; _} ->
      Destructured {
        default_structure;
        parts = NEList.singleton {melody = ""; chords = ""};
        transitions = [];
      }
end

type source_core = {
  source: Source.t Entry.Id.t;
  structure: Content.structure;
  details: string; [@default ""]
}
[@@deriving eq, yojson, show {with_path = false}]

type source = {
  source: Source.t Entry.t;
  structure: Content.structure;
  details: string;
}

let source_to_source_core : source -> source_core = fun {source; structure; details} ->
  {source = Entry.id source; structure; details}

let _key = "version"

type t = {
  tune: Tune.t Entry.Id.t;
  key: Music.Key.t;
  sources: source_core list; [@default []]
  arrangers: Person.t Entry.Id.t list; [@default []]
  remark: string; [@default ""]
  disambiguation: string; [@default ""]
  content: Content.t;
  (** In the client, we don't include the content, and it has to be retrieved by
      calling a specific endpoint; in the meantime, we fill it with [None]. *)
}
[@@deriving eq, yojson, make, show {with_path = false}, fields]

let make ~tune ~key ?sources ?arrangers ?remark ?disambiguation ~content () =
  let disambiguation = Option.map (String.remove_duplicates ~char: ' ') disambiguation in
  let tune = Entry.id tune in
  let sources = Option.map (List.map source_to_source_core) sources in
  let arrangers = Option.map (List.map Entry.id) arrangers in
  make ~tune ~key ?sources ?arrangers ?remark ?disambiguation ~content ()

let tune' = tune % Entry.value
let key' = key % Entry.value
let remark' = remark % Entry.value
let disambiguation' = disambiguation % Entry.value
let content' = content % Entry.value

let set_content content version =
  {version with content}

let erase_lilypond_from_content version =
  {version with content = Content.erase_lilypond version.content}
