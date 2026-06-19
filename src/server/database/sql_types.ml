open Dancelor_common

type kind_base = [`Jig | `Reel | `Strathspey | `Waltz | `Polka | `Jig_9_8 | `Other]

let kind_base_to_common : kind_base -> Kind_base.t = function
  | `Jig -> Jig
  | `Reel -> Reel
  | `Strathspey -> Strathspey
  | `Waltz -> Waltz
  | `Polka -> Polka
  | `Jig_9_8 -> Jig_9_8
  | `Other -> Other

let kind_base_of_common : Kind_base.t -> kind_base = function
  | Jig -> `Jig
  | Reel -> `Reel
  | Strathspey -> `Strathspey
  | Waltz -> `Waltz
  | Polka -> `Polka
  | Jig_9_8 -> `Jig_9_8
  | Other -> `Other

type two_chords = [`Dont_know | `One_chord | `Two_chords]

let two_chords_to_common : two_chords -> Model_builder.Core.Dance.two_chords = function
  | `Dont_know -> Dont_know
  | `One_chord -> One_chord
  | `Two_chords -> Two_chords

let two_chords_of_common : Model_builder.Core.Dance.two_chords -> two_chords = function
  | Dont_know -> `Dont_know
  | One_chord -> `One_chord
  | Two_chords -> `Two_chords
