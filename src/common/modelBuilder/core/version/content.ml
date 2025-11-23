open Nes

type destructured = {
  parts: Voices.t NEList.t;
  transitions: (Part_name.open_ * Part_name.open_ * Voices.t) list;
  default_structure: Structure.t;
}
[@@deriving eq, yojson, show {with_path = false}]

type t =
  | Monolithic of {lilypond: string; bars: int; structure: Structure.t}
  | Destructured of destructured
[@@deriving eq, yojson, show {with_path = false}, variants]

let lilypond kind key = function
  | Monolithic {lilypond; _} -> lilypond
  | Destructured {parts; _} -> Voices.lilypond_from_parts kind key parts

let erase_lilypond = function
  | Monolithic {bars; structure; _} -> Monolithic {bars; structure; lilypond = ""}
  | Destructured {default_structure; _} ->
    Destructured {
      default_structure;
      parts = NEList.singleton Voices.empty;
      transitions = [];
    }
