let _key = "date"

type t = int * int * int

let to_string date =
  NesPartialDate.(to_string (internal__from_full date))

let to_pretty_string date =
  NesPartialDate.(to_pretty_string (internal__from_full date))

let from_string str =
  match NesPartialDate.(internal__to_full (from_string str)) with
  | exception Failure _ -> failwith "NesDate.from_string"
  | None -> failwith "NesDate.from_string"
  | Some date -> date
