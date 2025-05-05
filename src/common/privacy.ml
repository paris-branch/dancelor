(** {1 Privacy}

    Levels of privacy are attached to entries in the database and influence who
    gets to see them. *)

type t =
  Private | Public

(* Private
     |
   Public *)

let default = Private

let eq = (=)

let gt a b =
  match a, b with
  | _, Private -> false
  | Private, _ -> true
  | _, Public -> false

let ge a b = eq a b || gt a b

let can_depend privacy ~on =
  not @@ gt on privacy

let to_string = function
  | Private -> "private"
  | Public -> "public"

let to_yojson privacy =
  `String (to_string privacy)

let pp fmt privacy =
  Format.pp_print_string fmt (to_string privacy)

let from_string = function
  | "private" -> Private
  | "public" -> Public
  | _ -> failwith "Common.Privacy.from_string"

let of_yojson = function
  | `String string ->
    (
      try
        Ok (from_string string)
      with
        | _ -> Error "Common.Privacy.of_yojson: not a valid privacy"
    )
  | _ -> Error "Common.Privacy.of_yojson: not a JSON string"
