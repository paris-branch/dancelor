open Nes

let _key = "set-order"

type component =
  | External of int
  | Internal of int

let component_to_string = function
  | External int -> spf "X%d" int
  | Internal int -> spf "%d" int

let component_to_pretty_string = component_to_string

let component_of_string =
  (* FIXME: Str might break when used from JS? *)
  let regexp = Str.regexp "[xX]\\([1-9][0-9]*\\)" in
  fun string ->
    match int_of_string_opt string with
    | Some index -> Internal index
    | None ->
      if Str.string_match regexp string 0 then
        External (int_of_string (Str.matched_group 1 string))
      else
        invalid_arg "Dancelor_common_model.SetOrder.component_of_string"

type t = component list

let to_string order =
  List.map component_to_string order
  |> String.concat ","

let to_yojson order =
  `String (to_string order)

let to_pretty_string order =
  let rec max_indice = function
    | [] -> 0
    | (External _) :: _ -> max_int (* we don't want externals *)
    | (Internal int) :: rest -> max int (max_indice rest)
  in
  let max_indice = max_indice order in
  if max_indice < 10 then
    List.map component_to_pretty_string order
    |> String.concat ""
  else
    to_string order

let of_string =
  (* FIXME: Str might break when used from JS? *)
  let regexp = Str.regexp "[ \t]*,[ \t]*" in
  fun string ->
    Str.split regexp string
    |> List.map component_of_string

let of_yojson = function
  | `String string ->
    (try Ok (of_string string)
     with _ -> Error "Dancelor_common_model.SetOrder.of_yojson: not a valid set order")
  | _ -> Error "Dancelor_common_model.SetOrder.of_yojson: not a JSON string"
