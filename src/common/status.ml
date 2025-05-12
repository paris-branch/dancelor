open Nes

type t =
  | Locked
  | ToBeConfirmed
  | Unlocked

(* Locked
     |
   ToBeConfirmed
     |
   Unlocked *)

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

let can_depend status ~on =
  ge on status

let to_string = function
  | Locked -> "locked"
  | ToBeConfirmed -> "to-be-confirmed"
  | Unlocked -> "unlocked"

let yojson_of_t status =
  `String (to_string status)

let pp fmt s =
  Format.pp_print_string fmt (to_string s)

let from_string = function
  | "locked" -> Locked
  | "to-be-confirmed" -> ToBeConfirmed
  | "unlocked" -> Unlocked
  | _ -> failwith "Common.Status.from_string"

let t_of_yojson = function
  | `String s ->
    (
      try
        from_string s
      with
        | _ -> Json.of_yojson_error "not a valid status" (`String s)
    )
  | j -> Json.of_yojson_error "not a JSON string" j
