let foi = float_of_int

let pf = Format.printf
let epf = Format.eprintf
let fpf = Format.fprintf
let spf = Format.sprintf
let aspf = Format.asprintf
let kspf = Format.ksprintf
let kaspf = Format.kasprintf

let ssf = Scanf.sscanf

type 'a seq = 'a Seq.t

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

let catch_and_wrap f =
  try Some (f ()) with _ -> None

let to_string_of_pp pp x =
  let buf = Buffer.create 8 in
  let fmt = Format.formatter_of_buffer buf in
  Format.fprintf fmt "%a@?" pp x;
  Buffer.contents buf

let max_l = function
  | [] -> failwith "NesPervasives.max_l"
  | h :: q -> List.fold_left max h q

let pmod a b = ((a mod b) + b) mod b
let pdiv a b = (a - pmod a b) / b

let%test _ = (pmod (-27) 12) + (pdiv (-27) 12) * 12 = -27
let%test _ = (pmod 27 (-12)) + (pdiv 27 (-12)) * -12 = 27
let%test _ = (pmod 7 7) + (pdiv 7 7) * 7 = 7
let%test _ = (pmod (-4) 67) + (pdiv (-4) 67) * 67 = -4
let%test _ = (pmod (-67) 4) + (pdiv (-67) 4) * 4 = -67
let%test _ = (pmod (-67) (-4)) + (pdiv (-67) (-4)) * -4 = -67

let compare_or cmp1 cmp2 =
  if cmp1 <> 0 then cmp1 else cmp2 ()

let rec first_non_zero ?(or_ = 0) = function
  | [] -> or_
  | first :: rest ->
    match first () with
    | 0 -> first_non_zero ~or_ rest
    | n -> n

let ( % ) f g = fun x -> f (g x)
let ( %> ) f g = fun x -> f x |> g

let pair x y = (x, y)
let map_fst f (x, y) = (f x, y)
let map_snd f (x, y) = (x, f y)
let map_pair f g (x, y) = (f x, g y)

type ('a, 'b) either = [%import: ('a, 'b) Either.t] [@@deriving yojson]

let rec fixpoint ?(eq = (=)) f x =
  let y = f x in
  if eq x y then
    x
  else
    fixpoint ~eq f y

let curry f = fun x y -> f (x, y)

let uncurry f = fun (x, y) -> f x y

(** A helper to generate unique identifiers that are somewhat readable. It is
    not cryptographically secure, but probably good enough. *)
let uid () =
  Format.sprintf
    "%Lx%Lx"
    (Random.int64_in_range ~min: Int64.min_int ~max: Int64.max_int)
    (Random.int64_in_range ~min: Int64.min_int ~max: Int64.max_int)

(* We really use the following all the time. *)

let flip = Fun.flip
let const = Fun.const
let const2 = NesFun.const2

let ok = Result.ok
let error = Result.error
let some = Option.some
let left = Either.left
let right = Either.right

let lwt = Lwt.return
let lwt_unit = Lwt.return_unit
let lwt_none = Lwt.return_none
let lwt_some = Lwt.return_some
let lwt_ok = Lwt.return_ok
let lwt_error = Lwt.return_error
let lwt_nil = Lwt.return_nil
let lwt_empty = Lwt.return ""
let lwt_true = Lwt.return_true
let lwt_false = Lwt.return_false
let lwt_left x = Lwt.return @@ left x
let lwt_right x = Lwt.return @@ right x

let (<$>) = Lwt.map
let (>>=) = Lwt.bind
let (=<<) f x = Lwt.bind x f
