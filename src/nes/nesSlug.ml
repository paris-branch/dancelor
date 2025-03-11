type 'a t = string option

let none = None

let is_none = (=) None

let check_string s =
  if NesString.is_slug s then
    Some (Some s)
  else
    None

exception NotASlug

let check_string_exn s =
  match check_string s with
  | Some s -> s
  | None -> raise NotASlug

let to_yojson _ = function
  | Some s -> `String s
  | None -> `Null

let of_yojson _ = function
  | `String s ->
    (
      match check_string s with
      | Some s -> Ok s
      | None -> Error ("NesSlug.of_yojson: '" ^ s ^ "' is a string but not a slug: " ^ NesString.slugify s)
    )
  | `Null -> Ok None
  | _ -> Error "NesSlug.of_yojson: is neither a string nor null"

let from_string string =
  if string = "" then
    raise (Invalid_argument "NesSlug.from_string");
  let slug = NesString.slugify string in
  if slug = "" then
    Some "-"
  else
    Some slug

let%test _ = from_string "Hello you, how are you?!" = Some "hello-you-how-are-you"
let%test _ = from_string "<> My friend!" = Some "lessgreater-my-friend"
let%test _ = from_string "*ù" = Some "u"
let%test _ = from_string "*&&" = Some "andand"

let equal' slug1 slug2 =
  match slug1, slug2 with
  | Some slug1, Some slug2 -> String.equal slug1 slug2
  | _ -> failwith "NesSlug.equal: cannot be None"

let equal _ slug1 slug2 = equal' slug1 slug2

let compare slug1 slug2 =
  match slug1, slug2 with
  | Some slug1, Some slug2 -> String.compare slug1 slug2
  | _ -> failwith "NesSlug.compare: cannot be None"

let to_string = function
  | Some s -> s
  | None -> failwith "NesSlug.to_string"

let pp' fmt slug =
  Format.pp_print_string fmt (to_string slug)

let pp _ = pp'

let unsafe_coerce = Fun.id
let unsafe_equal = equal'
let unsafe_of_string = Option.some

let compare_slugs_or ~fallback slug x y =
  let slug_x = slug x in
  let slug_y = slug y in
  if not (is_none slug_x) && not (is_none slug_y) then
    compare slug_x slug_y
  else
    fallback x y
