open Nes

let _key = "set-order"

type component =
  | External of int
  | Internal of int
[@@deriving show {with_path = false}]

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
[@@deriving show {with_path = false}]

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
    let rec components_to_pretty_strings ~previous = function
      | [] -> []
      | (Internal n) :: rest when n < previous && rest <> [] ->
        (" " ^ component_to_pretty_string (Internal n))
        :: components_to_pretty_strings ~previous:n rest
      | (External n) :: rest ->
        (" " ^ component_to_pretty_string (External n))
        :: components_to_pretty_strings ~previous:max_int rest
      | (Internal n) :: rest ->
        component_to_pretty_string (Internal n)
        :: components_to_pretty_strings ~previous:n rest
    in
    components_to_pretty_strings ~previous:0 order
    |> String.concat ""
  else
    to_string order

let of_string =
  let regexp = Str.regexp "[ \t]*,[ \t]*" in
  fun string ->
    let result =
      Str.split regexp string
      |> List.map component_of_string
    in
    if List.length result = 0 then
      failwith "SetOrder.of_string"
    else
      result

let of_string_opt s =
  try Some (of_string s) with _ -> None

let check ?number s =
  match of_string_opt s with
  | None -> false
  | Some o ->
    match number with
    | None -> true
    | Some n ->
      (* Check that the n tunes are mentioned *)
      let present = Array.make n false in
      List.iter
        (function
          | External _ -> ()
          | Internal i -> present.(i-1) <- true)
        o;
      Array.for_all Fun.id present

let of_yojson = function
  | `String string ->
    (try Ok (of_string string)
     with _ -> Error "Dancelor_common_model.SetOrder.of_yojson: not a valid set order")
  | _ -> Error "Dancelor_common_model.SetOrder.of_yojson: not a JSON string"
