include List

let hd_opt = function
  | [] -> None
  | h :: _ -> Some h

let hd_tl = function
  | [] -> failwith "hd_tl"
  | x :: xs -> (x, xs)

(* Bodies and feet. *)

let bd_ft_opt xs =
  let rec bd_ft_opt acc = function
    | [] -> None
    | [x] -> Some (List.rev acc, x)
    | x :: xs -> bd_ft_opt (x :: acc) xs
  in
  bd_ft_opt [] xs

let bd_ft xs =
  match bd_ft_opt xs with
  | Some result -> result
  | None -> failwith "bd_ft"

let ft_opt l = Option.map snd (bd_ft_opt l)

let ft l =
  match ft_opt l with
  | Some x -> x
  | _ -> failwith "ft"

let interspersei ?last mid l =
  let rec interspersei i = function
    | [] -> []
    | [e] -> [e]
    | [e; f] -> [e; Option.value ~default: mid last i; f]
    | h :: t -> h :: mid i :: interspersei (i + 1) t
  in
  interspersei 0 l

let intersperse ?last mid =
  interspersei ?last: (Option.map Fun.const last) (Fun.const mid)

let%test _ = intersperse "," ["a"; "b"] = ["a"; ","; "b"]
let%test _ = intersperse "," ~last: "and" ["a"; "b"] = ["a"; "and"; "b"]
let%test _ = intersperse "," ["a"; "b"; "c"] = ["a"; ","; "b"; ","; "c"]
let%test _ = intersperse "," ~last: "and" ["a"; "b"; "c"] = ["a"; ","; "b"; "and"; "c"]
let%test _ = intersperse "," ["a"; "b"; "c"; "d"] = ["a"; ","; "b"; ","; "c"; ","; "d"]
let%test _ = intersperse "," ~last: "and" ["a"; "b"; "c"; "d"] = ["a"; ","; "b"; ","; "c"; "and"; "d"]

let singleton x = [x]

let sort_count cmp l =
  (* count_duplicates assumes that the list is sorted *)
  let rec count_duplicates acc (prev, nb) = function
    | [] -> rev ((prev, nb) :: acc)
    | h :: t ->
      if cmp h prev = 0 then
        count_duplicates acc (prev, nb + 1) t
      else
        count_duplicates ((prev, nb) :: acc) (h, 1) t
  in
  match sort cmp l with
  | [] -> []
  | h :: t -> count_duplicates [] (h, 1) t

(******************************************************************************)
(* Lwt-Aware Sorting functions                                                *)
(*                                                                            *)
(* The tests for these functions can be found in test/nesListTest.ml. They    *)
(* not inline because that would require lwt.unix which clashes with          *)
(* js_of_ocaml.                                                               *)

type 'a context = {
  element: 'a;
  index: int;
  previous: 'a option;
  next: 'a option;
  total: int;
}

let findi_context p l =
  let rec findi_context previous index = function
    | [] -> None
    | element :: l when p index element ->
      Some {element; previous; index; next = hd_opt l; total = 1 + index + length l}
    | x :: l -> findi_context (Some x) (index + 1) l
  in
  findi_context None 0 l

let find_context p = findi_context (Fun.const p)

let map_context f c = {
  element = f c.element;
  index = c.index;
  previous = Option.map f c.previous;
  next = Option.map f c.next;
  total = c.total;
}

let all_some l =
  match partition_map (function Some x -> Left x | _ -> Right ()) l with
  | (l, []) -> Some l
  | _ -> None

let remove i l =
  let rec remove i acc = function
    | [] -> List.rev acc
    | h :: t -> if i = 0 then List.rev_append acc t else remove (i - 1) (h :: acc) t
  in
  remove i [] l

let replace i x l =
  let rec replace i acc = function
    | [] -> invalid_arg "replace"
    | h :: t when i = 0 -> (h, List.rev_append acc (x :: t))
    | h :: t -> replace (i - 1) (h :: acc) t
  in
  replace i [] l

let swap i j l =
  let rec swap i j acc = function
    | [] -> invalid_arg "swap"
    | h :: t when i = 0 ->
      let (h, t) = replace (j - 1) h t in
      List.rev_append acc (h :: t)
    | h :: t ->
      swap (i - 1) (j - 1) (h :: acc) t
  in
  if i = j then l
  else if i < j then swap i j [] l
  else swap j i [] l

let rec extract_assoc x = function
  | [] -> raise Not_found
  | (k, v) :: xs when x = k -> (v, xs)
  | kv :: xs -> NesPair.map_snd (List.cons kv) (extract_assoc x xs)

let extract_assoc_opt x l =
  try Some (extract_assoc x l) with Not_found -> None

let rec map_first_some f = function
  | [] -> None
  | x :: xs -> match f x with None -> map_first_some f xs | Some y -> Some y

let to_option ?(more = fun _ -> invalid_arg "NesList.to_option") = function
  | [] -> None
  | [x] -> Some x
  | xs -> more xs

let group ~by l =
  (* this could really be made way more efficient, but since it will be used
     on groups of two or three elements, it does not matter *)
  let rec aux gs = function
    | [] -> gs
    | x :: xs ->
      let (gs, found) =
        List.fold_left
          (fun (gs, found) g ->
            if not found && by x (hd g) then
                ((x :: g) :: gs, true)
            else
                (g :: gs, found)
          )
          ([], false)
          (rev gs)
      in
      if found then aux gs xs
      else aux ([x] :: gs) xs
  in
  aux [] l

let contains_duplicates ?(eq = Stdlib.(=)) xs =
  let rec aux = function
    | [] -> false
    | x :: xs when List.exists (eq x) xs -> true
    | _ :: xs -> aux xs
  in
  aux xs

let rec take n = function
  | [] -> []
  | _ when n <= 0 -> []
  | x :: xs -> x :: take (n - 1) xs
