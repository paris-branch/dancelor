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

let erase_lilypond = function
  | Monolithic {bars; structure; _} -> Monolithic {bars; structure; lilypond = ""}
  | Destructured {default_structure; _} ->
    Destructured {
      default_structure;
      parts = NEList.singleton Voices.empty;
      transitions = [];
    }

let lilypond ?structure: desired_structure ~kind ~key parts transitions =
  let transitions =
    (* rearrange the given transitions because we will want to List.assoc later *)
    List.map (fun (p1, p2, t) -> ((p1, p2), t)) transitions
  in
  let time =
    match kind with
    | Kind.Base.Reel -> "2/2"
    | Jig -> "6/8"
    | Strathspey -> "4/4"
    | Waltz -> "3/4"
    | Polka -> "2/2"
  in
  let key =
    (Music.Pitch.to_lilypond_string @@ Music.Key.pitch key) ^
    " " ^ (Music.Mode.to_lilypond_string @@ Music.Key.mode key)
  in
  let parts = NEList.to_list parts in
  let Voices.{melody; chords} =
    let structure = Option.bind desired_structure Structure.best_fold_for in
    let show_part_marks = structure = None in
    let transition ~toplevel from to_ =
      (* NOTE: The transition from start and to the end should only ever be
         considered when at toplevel of the structure; otherwise they would be
         produced at all levels of imbricated repeats. *)
      if (from = Part_name.Start || to_ = Part_name.End) && not toplevel then None
      else List.assoc_opt (from, to_) transitions
    in
    let rec item_to_lilypond ~toplevel ~next_part = function
      | Structure.Part part ->
        (
          let lilypond = List.nth parts (Part_name.to_int part) in
          let lilypond = if show_part_marks then Voices.concat (Voices.mark part) lilypond else lilypond in
          match transition ~toplevel (Middle part) next_part with
          | None -> lilypond
          | Some transition -> Voices.concat_l [lilypond; Voices.space; transition]
        )
      | Repeat (times, structure) ->
        (
          let lilypond : Voices.t = to_lilypond ~toplevel: false structure in
          let first_part = Structure.first_part_exn structure in
          let last_part = Structure.last_part_exn structure in
          let alt_1 = Option.value ~default: Voices.empty @@ transition ~toplevel (Middle last_part) (Middle first_part) in
          let alt_2 = Option.value ~default: Voices.empty @@ transition ~toplevel (Middle last_part) next_part in
          {
            Voices.melody =
            spf
              "\\repeat volta %d { %s } \\alternative { { %s } { %s } }"
              times
              lilypond.melody
              alt_1.melody
              alt_2.melody;
            chords =
            spf
              "%s %s %s"
              lilypond.chords
              alt_1.chords
              alt_2.chords;
          }
        )
    and map_item_to_lilypond ~toplevel = function
      | [] -> []
      | item :: items ->
        let next_part =
          Option.fold
            ~none: Part_name.End
            ~some: Part_name.middle
            (Structure.first_part items)
        in
        item_to_lilypond ~toplevel ~next_part item :: map_item_to_lilypond ~toplevel items
    and to_lilypond ~toplevel structure =
      Voices.concat_l (List.intersperse Voices.section_break (map_item_to_lilypond ~toplevel structure))
    in
    let to_lilypond structure =
      let lilypond = to_lilypond ~toplevel: true structure in
      let first_part = Structure.first_part_exn structure in
      match transition ~toplevel: true Start (Middle first_part) with
      | None -> lilypond
      | Some transition -> Voices.concat_l [transition; Voices.space; lilypond]
    in
    let structure = Option.value structure ~default: (List.mapi (fun n _ -> Structure.part @@ Part_name.of_int n) parts) in
    Voices.concat (to_lilypond structure) Voices.fine
  in
  spf
    "<< \\new Voice {\\clef treble \\time %s \\key %s {%s}}\\new ChordNames {\\chordmode {%s}}>>"
    time
    key
    melody
    chords

let lilypond ?structure ~kind ~key = function
  | Monolithic {lilypond; _} -> lilypond
  | Destructured {parts; transitions; _} -> lilypond ?structure ~kind ~key parts transitions
