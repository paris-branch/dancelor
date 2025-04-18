open NesPervasives

module I = ISO8601.Permissive

type t = float
(* Number of seconds since 00:00:00 GMT, Jan. 1, 1970.*)

let of_string = I.datetime ~reqtime: true
let to_string = I.string_of_datetime

let pp fmt = fpf fmt "%s" % to_string

let to_yojson date =
  `String (to_string date)

let of_yojson = function
  | `String s ->
    (
      try
        Ok (of_string s)
      with
        | _ -> Error "NesDatetime.of_yojson: not a valid datetime"
    )
  | _ -> Error "NesDatetime.of_yojson: not a JSON string"

let now = Unix.gettimeofday

let diff = (-.)
