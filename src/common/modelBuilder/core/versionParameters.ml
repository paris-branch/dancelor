(** {1 Version Parameters}

    This module defines parameters that make sense at the level of a version. *)

open Nes

type t = {
  transposition: Transposition.t option; [@default None]
  first_bar: int option; [@default None] [@key "first-bar"]
  clef: Music.Clef.t option; [@default None]
  structure: Version.Content.structure option; [@default None]
  trivia: string option; [@default None]
  display_name: NEString.t option; [@default None] [@key "display-name"]
  display_composer: NEString.t option [@default None] [@key "display-composer"]
}
[@@deriving eq, make, show {with_path = false}, yojson, fields]

(* FIXME: [@@deriving yojson] and [@@deriving make] do not have the same
   interpretation of [@default]. Basically, we want [@@deriving yojson] to
   interpret it, but not [@@deriving make]. This is normally possible by using
   [@yojson.default]. Current version of [@@deriving yojson] (3.5.3) does not,
   however, seem to recognise this option anymore. In the meantime, we use
   [@default] and we add a dirty fix for [@@deriving make]: *)
let make ?transposition ?clef ?structure ?first_bar ?display_name ?display_composer () =
  make ~transposition ~clef ~structure ~first_bar ~display_name ~display_composer ()

(** {2 Defaults}

    Getters that end with a quote are getters that have a default value. *)

let none = `Assoc [] |> of_yojson |> Result.get_ok

let first_bar' = Option.value ~default: 1 % first_bar
let trivia' ~default = Option.value ~default % trivia

(** {2 Setters} *)

let set_display_name display_name p = {p with display_name = Some display_name}

(** {2 Composition} *)

let compose first second = {
  transposition = Option.choose ~tie: Transposition.compose first.transposition second.transposition;
  clef = Option.(choose ~tie: second) first.clef second.clef;
  structure = Option.(choose ~tie: second) first.structure second.structure;
  first_bar = Option.(choose ~tie: second) first.first_bar second.first_bar;
  trivia = Option.(choose ~tie: second) first.trivia second.trivia;
  display_name = Option.(choose ~tie: second) first.display_name second.display_name;
  display_composer = Option.(choose ~tie: second) first.display_composer second.display_composer
}
