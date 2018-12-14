include Pervasives

let (||>) f g x = f x |> g

let id = fun x -> x

let in_channel_to_string ic =
  let all = Buffer.create 8 in
  let bufsize = 1024 in
  let buf = Bytes.create bufsize in
  let rec aux () =
    match input ic buf 0 bufsize with
    | 0 -> ()
    | n ->
       Buffer.add_subbytes all buf 0 n;
       aux ()
  in
  aux ();
  Buffer.contents all

let escape_shell_argument =
  String.split_on_char '\''
  ||> String.concat "'\\''"
  ||> fun s -> "'" ^ s ^ "'"

let catch_and_wrap f =
  try Some (f ()) with _ -> None

let spf = Format.sprintf

let pp_string_multiline fmt s =
  match String.split_on_char '\n' s with
  | [] -> ()
  | l :: ls ->
     Format.fprintf fmt "%s" l;
     List.iter (Format.fprintf fmt "@\n%s") ls
(** Pretty-printer that replaces straight '\n' by format cuts '@\n'. *)
