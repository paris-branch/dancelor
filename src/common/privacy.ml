(** {1 Privacy}

    Levels of privacy are attached to entries in the database and influence who
    gets to see them. *)

open Nes

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

let yojson_of_t privacy =
  `String (to_string privacy)

let pp fmt privacy =
  Format.pp_print_string fmt (to_string privacy)

let from_string = function
  | "private" -> Private
  | "public" -> Public
  | _ -> failwith "Common.Privacy.from_string"

let t_of_yojson = function
  | `String s ->
    (
      try
        from_string s
      with
        | _ -> Json.of_yojson_error "not a valid privacy" (`String s)
    )
  | j -> Json.of_yojson_error "not a JSON string" j
