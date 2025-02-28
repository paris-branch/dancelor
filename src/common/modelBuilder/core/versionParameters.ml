(** {1 Version Parameters}

    This module defines parameters that make sense at the level of a version. *)

open Nes

module Self = struct
  type t = {
    transposition: Transposition.t option; [@default None]
    first_bar: int option; [@default None] [@key "first-bar"]
    for_dance: Dance.t Slug.t option; [@default None] [@key "for-dance"]
    instruments: string option; [@default None]
    clef: Music.clef option; [@default None]
    trivia: string option; [@default None]
    display_name: string option; [@default None] [@key "display-name"]
    display_composer: string option [@default None] [@key "display-composer"]
  }
  [@@deriving eq, make, show {with_path = false}, yojson]
end
include Self

(* FIXME: [@@deriving yojson] and [@@deriving make] do not have the same
   interpretation of [@default]. Basically, we want [@@deriving yojson] to
   interpret it, but not [@@deriving make]. This is normally possible by using
   [@yojson.default]. Current version of [@@deriving yojson] (3.5.3) does not,
   however, seem to recognise this option anymore. In the meantime, we use
   [@default] and we add a dirty fix for [@@deriving make]: *)
let make ?instruments ?transposition ?clef ?first_bar ?display_name ?display_composer () =
  make ~instruments ~transposition ~clef ~first_bar ~display_name ~display_composer ()

let make_instrument pitch =
  make
    ~instruments: (Music.pitch_to_pretty_string pitch ^ " instruments")
    ~transposition: (Transposition.relative pitch Music.pitch_c)
    ()

(** {2 Getters} *)

let transposition p = p.transposition
let first_bar p = p.first_bar
let instruments p = p.instruments
let for_dance p = p.for_dance
let clef p = p.clef
let trivia p = p.trivia
let display_name p = p.display_name
let display_composer p = p.display_composer

(** {2 Defaults}

    Getters that end with a quote are getters that have a default value. *)

let none = `Assoc [] |> of_yojson |> Result.get_ok

let transposition' = Option.value ~default: Transposition.identity % transposition
let first_bar' = Option.value ~default: 1 % first_bar
let display_name' ~default = Option.value ~default % display_name
let display_composer' ~default = Option.value ~default % display_composer
let trivia' ~default = Option.value ~default % trivia

(** {2 Setters} *)

let set_display_name display_name p = {p with display_name = Some display_name}

(** {2 Composition} *)

let compose first second = {
  instruments = Option.(choose ~tie: fail) first.instruments second.instruments;
  transposition = Option.choose ~tie: Transposition.compose first.transposition second.transposition;
  clef = Option.(choose ~tie: second) first.clef second.clef;
  first_bar = Option.(choose ~tie: second) first.first_bar second.first_bar;
  for_dance = Option.(choose ~tie: fail) first.for_dance second.for_dance;
  trivia = Option.(choose ~tie: second) first.trivia second.trivia;
  display_name = Option.(choose ~tie: second) first.display_name second.display_name;
  display_composer = Option.(choose ~tie: second) first.display_composer second.display_composer
}
