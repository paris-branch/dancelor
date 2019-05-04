type t =
  | WorkInProgress
  | ToBeConfirmed
  | Locked

let to_string = function
  | WorkInProgress -> "work-in-progress"
  | ToBeConfirmed -> "to-be-confirmed"
  | Locked -> "locked"

let to_yojson status =
  `String (to_string status)

let from_string = function
  | "work-in-progress" -> WorkInProgress
  | "to-be-confirmed" -> ToBeConfirmed
  | "locked" -> Locked
  | _ -> failwith "Dancelor_common_model.Status.from_string"

let of_yojson = function
  | `String string ->
    (try Ok (from_string string)
     with _ -> Error "Dancelor_common_model.Status.of_yojson: not a valid status")
  | _ -> Error "Dancelor_common_model.Status.of_yojson: not a JSON string"
