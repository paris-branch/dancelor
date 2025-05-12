type t = int * int * int

let to_string date =
  NesPartialDate.(to_string (internal__from_full date))

let to_pretty_string ?at date =
  NesPartialDate.(to_pretty_string ?at (internal__from_full date))

let from_string str =
  match NesPartialDate.(Option.bind (from_string str) internal__to_full) with
  | None -> failwith "NesDate.from_string"
  | Some date -> date

let yojson_of_t date =
  `String (to_string date)

let t_of_yojson = function
  | `String s ->
    (
      try
        from_string s
      with
        | _ -> NesJson.of_yojson_error "not a valid date" (`String s)
    )
  | j -> NesJson.of_yojson_error "not a JSON string" j

let today () =
  let tm = Unix.(localtime (time ())) in
    (1900 + tm.tm_year, 1 + tm.tm_mon, tm.tm_mday)
