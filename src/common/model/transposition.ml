open Nes

module Self = struct
  type t =
    | Relative of Music.pitch * Music.pitch
    | Absolute of Music.pitch
  [@@deriving show {with_path = false}, yojson]

  let _key = "transposition"
end
include Self

let relative a b = Relative (a, b)
let absolute a = Absolute a

let identity = relative Music.pitch_c Music.pitch_c

let music_pitch_to_int pitch =
  (
    match Music.pitch_note pitch with
    | C -> 0
    | D -> 2
    | E -> 4
    | F -> 5
    | G -> 7
    | A -> 9
    | B -> 11
  ) +
    (
      match Music.pitch_alteration pitch with
      | Flat -> -1
      | Natural -> 0
      | Sharp -> +1
    ) +
    (Music.pitch_octave pitch * 12)

let music_pitch_of_int_array = [|
  Music.make_pitch C Natural;
  Music.make_pitch C Sharp;
  Music.make_pitch D Natural;
  Music.make_pitch D Sharp;
  Music.make_pitch E Natural;
  Music.make_pitch F Natural;
  Music.make_pitch F Sharp;
  Music.make_pitch G Natural;
  Music.make_pitch G Sharp;
  Music.make_pitch A Natural;
  Music.make_pitch A Sharp;
  Music.make_pitch B Natural;
|]

let music_pitch_of_int int =
  music_pitch_of_int_array.(pmod int 12) (pdiv int 12)

let%test _ = (-7 |> music_pitch_of_int |> music_pitch_to_int) = -7
let%test _ = (-6 |> music_pitch_of_int |> music_pitch_to_int) = -6
let%test _ = (-5 |> music_pitch_of_int |> music_pitch_to_int) = -5
let%test _ = (-4 |> music_pitch_of_int |> music_pitch_to_int) = -4
let%test _ = (-3 |> music_pitch_of_int |> music_pitch_to_int) = -3
let%test _ = (-2 |> music_pitch_of_int |> music_pitch_to_int) = -2
let%test _ = (-1 |> music_pitch_of_int |> music_pitch_to_int) = -1
let%test _ = (0 |> music_pitch_of_int |> music_pitch_to_int) = 0
let%test _ = (1 |> music_pitch_of_int |> music_pitch_to_int) = 1
let%test _ = (2 |> music_pitch_of_int |> music_pitch_to_int) = 2
let%test _ = (3 |> music_pitch_of_int |> music_pitch_to_int) = 3
let%test _ = (4 |> music_pitch_of_int |> music_pitch_to_int) = 4
let%test _ = (5 |> music_pitch_of_int |> music_pitch_to_int) = 5
let%test _ = (6 |> music_pitch_of_int |> music_pitch_to_int) = 6
let%test _ = (7 |> music_pitch_of_int |> music_pitch_to_int) = 7

let music_pitch_transpose ~source ~target p =
  music_pitch_of_int
    (
      music_pitch_to_int target -
        music_pitch_to_int source +
        music_pitch_to_int p
    )

let compose trans1 trans2 =
  match trans2 with
  | Absolute _ -> trans2
  | Relative (source2, target2) ->
    match trans1 with
    | Absolute target1 ->
      Absolute (music_pitch_transpose ~source: source2 ~target: target2 target1)
    | Relative (source1, target1) ->
      Relative (source1, music_pitch_transpose ~source: source2 ~target: target2 target1)
