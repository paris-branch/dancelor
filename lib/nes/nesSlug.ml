type 'a t = string option

let none = None

let is_none = (=) None

let to_yojson _ = function
  | Some s -> `String s
  | None -> `Null

let of_yojson _ = function
  | `String s -> Ok (Some s)
  | `Null -> Ok None
  | _ -> Error "NesSlug.of_yojson"

let from_string string =
  if string = "" then
    raise (Invalid_argument "NesSlug.from_string");
  let slug = Slug.slugify string in
  if slug = "" then
    Some "-"
  else
    Some slug

let%test _ = from_string "Hello you, how are you?!" = Some "hello-you-how-are-you"
let%test _ = from_string "<> My friend!" = Some "lessgreater-my-friend"
let%test _ = from_string "*ù" = Some "u"
let%test _ = from_string "*&&" = Some "andand"

let equal slug1 slug2 =
  match slug1, slug2 with
  | Some slug1, Some slug2 -> String.equal slug1 slug2
  | _ -> failwith "NesSlug.equal: cannot be None"

let compare slug1 slug2 =
  match slug1, slug2 with
  | Some slug1, Some slug2 -> String.compare slug1 slug2
  | _ -> failwith "NesSlug.compare: cannot be None"

let to_string = function
  | Some s -> s
  | None -> failwith "NesSlug.to_string"

let pp fmt slug =
  Format.pp_print_string fmt (to_string slug)

let slugify string = Slug.slugify string
let unsafe_of_string str = Some str
