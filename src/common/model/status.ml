type t =
  | WorkInProgress
  | ToBeConfirmed
  | Locked

let to_string = function
  | WorkInProgress -> "work-in-progress"
  | ToBeConfirmed -> "to-be-confirmed"
  | Locked -> "locked"

let to_jsonm status =
  `String (to_string status)

let from_string = function
  | "work-in-progress" -> WorkInProgress
  | "to-be-confirmed" -> ToBeConfirmed
  | "locked" -> Locked
  | _ -> failwith "Dancelor_model.Status.from_string"

let of_jsonm = function
  | `String string -> from_string string
  | _ -> failwith "Dancelor_model.Status.of_jsonm"
