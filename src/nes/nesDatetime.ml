open NesPervasives

module I = ISO8601.Permissive

type t = float
(* Number of seconds since 00:00:00 GMT, Jan. 1, 1970.*)

let of_string = I.datetime ~reqtime: true
let to_string = I.string_of_datetime

let pp fmt = fpf fmt "%s" % to_string

let yojson_of_t date =
  `String (to_string date)

let t_of_yojson = function
  | `String s ->
    (
      try
        of_string s
      with
        | _ -> NesJson.of_yojson_error "not a valid datetime" (`String s)
    )
  | j -> NesJson.of_yojson_error "not a JSON string" j

let now = Unix.gettimeofday

let plus = (+.)

let diff = (-.)

let in_the_past t =
  (t -. now ()) < 0.

let make_in_the_future d =
  now () +. d
