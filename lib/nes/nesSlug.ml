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

let from_string str =
  if str = "" then
    raise (Invalid_argument "NesSlug.from_string");
  let str = String.lowercase_ascii str in
  let len = String.length str in
  let out = Bytes.create len in
  let j = ref 0 in
  let last_letter = ref (-10) in
  for i = 0 to len - 1 do
    if ('a' <= str.[i] && str.[i] <= 'z') || ('0' <= str.[i] && str.[i] <= '9') then
      (
        Bytes.set out !j str.[i];
        last_letter := !j;
        incr j
      )
    else if !last_letter = !j - 1 then
      (
        Bytes.set out !j '-';
        incr j
      )
  done;
  if !last_letter < 0 then
    Some "-"
  else
    Some (Bytes.sub_string out 0 (!last_letter+1))

let%test _ = from_string "Hello you, how are you?!" = Some "hello-you-how-are-you"
let%test _ = from_string "<> My friend!" = Some "my-friend"
let%test _ = from_string "*ù" = Some "-"

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

let unsafe_of_string str = Some str
