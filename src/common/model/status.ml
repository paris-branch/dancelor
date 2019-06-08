type t =
  | Locked
  | ToBeConfirmed
  | Unlocked

(*     Locked
         |
    ToBeConfirmed
         |
      Unlocked     *)

let bot = Unlocked
let top = Locked

let eq = (=)

let gt a b =
  match a, b with
  | _, Locked -> false
  | Locked, _ -> true
  | _, ToBeConfirmed -> false
  | ToBeConfirmed, _ -> true
  | _, Unlocked -> false

let ge a b = eq a b || gt a b

let _key = "status"

let to_string = function
  | Locked -> "locked"
  | ToBeConfirmed -> "to-be-confirmed"
  | Unlocked -> "unlocked"

let to_yojson status =
  `String (to_string status)

let from_string = function
  | "locked" -> Locked
  | "to-be-confirmed" -> ToBeConfirmed
  | "unlocked" -> Unlocked
  | _ -> failwith "Dancelor_common_model.Status.from_string"

let of_yojson = function
  | `String string ->
    (try Ok (from_string string)
     with _ -> Error "Dancelor_common_model.Status.of_yojson: not a valid status")
  | _ -> Error "Dancelor_common_model.Status.of_yojson: not a JSON string"
