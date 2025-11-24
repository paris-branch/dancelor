open Nes

type t = Part_name.t NEList.t
[@@deriving eq, show {with_path = false}]

let to_string = NEString.of_string_exn % String.of_seq % Seq.map Part_name.to_char % List.to_seq % NEList.to_list
let of_string = Option.join % Option.map NEList.of_list % Monadise.Option.monadise_1_1 List.map Part_name.of_char % List.of_seq % String.to_seq % NEString.to_string

let to_yojson s = `String (NEString.to_string @@ to_string s)
let of_yojson = function
  | `String s -> Option.to_result ~none: "not a valid structure" (Option.bind (NEString.of_string s) of_string)
  | _ -> Error "not a string"

type folded_item = Part of Part_name.t | Repeat of int * folded
and folded = folded_item list

(** A few helpers *)
let a = Part (Part_name.of_char_exn 'A')
let b = Part (Part_name.of_char_exn 'B')
let c = Part (Part_name.of_char_exn 'C')
let d = Part (Part_name.of_char_exn 'D')
let e = Part (Part_name.of_char_exn 'E')
let f = Part (Part_name.of_char_exn 'F')

let part n = Part n
let repeat n x = Repeat (n, x)

let rec first_part_exn structure =
  match List.hd_opt structure with
  | None -> failwith "first_part_exn"
  | Some Part a -> a
  | Some Repeat (_, structure) -> first_part_exn structure

let first_part structure =
  match List.hd_opt structure with
  | None -> None
  | Some Part a -> Some a
  | Some Repeat (_, structure) -> Some (first_part_exn structure)

let rec last_part_exn structure =
  match List.ft_opt structure with
  | None -> failwith "last_part_exn"
  | Some Part a -> a
  | Some Repeat (_, structure) -> last_part_exn structure

let last_part structure =
  match List.ft_opt structure with
  | None -> None
  | Some Part a -> Some a
  | Some Repeat (_, structure) -> Some (last_part_exn structure)

(** A general algorithm would be great, but for now this will do. *)
let best_fold_for structure =
  match NEString.to_string @@ to_string structure with
  | "A" -> some [a]
  | "B" -> some [b]
  | "C" -> some [c]
  | "D" -> some [d]
  | "AB" -> some [a; b]
  | "AAB" -> some [repeat 2 [a]; b]
  | "ABB" -> some [a; repeat 2 [b]]
  | "ABC" -> some [a; b; c]
  | "AAAA" -> some [repeat 4 [a]]
  | "AAAB" -> some [repeat 3 [a]; b]
  | "AABA" -> some [repeat 2 [a]; b; a]
  | "AABB" -> some [repeat 2 [a]; repeat 2 [b]]
  | "ABAA" -> some [a; b; repeat 2 [a]]
  | "ABAB" -> some [repeat 2 [a; b]]
  | "ABBA" -> some [a; repeat 2 [b]; a]
  | "ABBB" -> some [a; repeat 3 [b]]
  | "AABC" -> some [repeat 2 [a]; b; c]
  | "ABCD" -> some [a; b; c; d]
  | "AABBB" -> some [repeat 2 [a]; repeat 3 [b]]
  | "ABABA" -> some [repeat 2 [a; b]; a]
  | "ABABB" -> some [repeat 2 [a; b]; b]
  | "ABBAB" -> some [a; repeat 2 [b]; a; b]
  | "ABBCC" -> some [a; repeat 2 [b]; repeat 2 [c]]
  | "ABABC" -> some [repeat 2 [a; b]; c]
  | "ABCDE" -> some [a; b; c; d; e]
  | "ABABAB" -> some [repeat 3 [a; b]]
  | "AABBAB" -> some [repeat 2 [a]; repeat 2 [b]; a; b]
  | "AABBCC" -> some [repeat 2 [a]; repeat 2 [b]; repeat 2 [c]]
  | "ABCDEF" -> some [a; b; c; d; e; f]
  | "AABBCCDD" -> some [repeat 2 [a]; repeat 2 [b]; repeat 2 [c]; repeat 2 [d]]
  | "AABCDDEF" -> some [repeat 2 [a]; b; c; repeat 2 [d]; e; f]
  | _ -> None
