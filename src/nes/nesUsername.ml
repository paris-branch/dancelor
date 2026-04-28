type t = string [@@deriving eq, ord, show, to_yojson]

let from_string s =
  let s = String.trim s in
  let len = String.length s in
  (* Username constraints:
     - Not empty
     - Between 3 and 24 characters (byte length for UTF-8)
     - No control characters or whitespace
     - Must not start with underscore or hyphen *)
  if len < 3 || len > 24 then
    None
  else if String.exists (fun c -> Char.code c < 32 || Char.code c = 127) s then
    None
  else if String.contains s ' ' || String.contains s '\t' || String.contains s '\n' || String.contains s '\r' then
    None
  else if String.length s > 0 && (s.[0] = '_' || s.[0] = '-') then
    None
  else
    Some s

let to_string t = t
let to_nestring t = NesNEString.of_string_exn t

let of_string_exn s =
  match from_string s with
  | Some t -> t
  | None -> invalid_arg (Format.sprintf "Invalid username: %S" s)

let of_yojson = function
  | `String s ->
    (
      match from_string s with
      | Some t -> Ok t
      | None -> Error (Format.sprintf "Invalid username: %S" s)
    )
  | _ -> Error "Expected string for username"
