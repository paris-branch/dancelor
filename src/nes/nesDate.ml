let _key = "date"

type t = int * int * int

let to_string date =
  NesPartialDate.(to_string (internal__from_full date))

let to_pretty_string ?at date =
  NesPartialDate.(to_pretty_string ?at (internal__from_full date))

let from_string str =
  match NesPartialDate.(Option.bind (from_string str) internal__to_full) with
  | None -> failwith "NesDate.from_string"
  | Some date -> date

let to_yojson date =
  `String (to_string date)

let of_yojson = function
  | `String s ->
    (
      try
        Ok (from_string s)
      with
      | _ -> Error "NesDate.of_yojson: not a valid date"
    )
  | _ -> Error "NesDate.of_yojson: not a JSON string"

let today () =
  let tm = Unix.(localtime (time ())) in
  (1900 + tm.tm_year, 1 + tm.tm_mon, tm.tm_mday)
