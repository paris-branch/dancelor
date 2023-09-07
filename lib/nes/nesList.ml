include List

let rec map_filter f = function
  | [] -> []
  | h :: t ->
    match f h with
    | None -> map_filter f t
    | Some x -> x :: map_filter f t

let rec sub n l =
  if n <= 0 then []
  else
    (
      match l with
      | [] -> []
      | h :: t -> h :: (sub (n - 1) t)
    )

let hd_opt = function
  | [] -> None
  | h :: _ -> Some h

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

let bd_opt l = Option.map fst (bd_ft_opt l)

let bd l =
  match bd_opt l with
  | Some l -> l
  | _ -> failwith "bd"

let ft_opt l = Option.map snd (bd_ft_opt l)

let ft l =
  match ft_opt l with
  | Some x -> x
  | _ -> failwith "ft"

let intertwine f l =
  let rec intertwine i = function
    | [] -> []
    | [e] -> [e]
    | h :: q ->
      h :: (f i) :: intertwine (i + 1) q
  in
  intertwine 0 l

let singleton x = [x]

let rec compare_lwt cmp l1 l2 =
  match l1, l2 with
  | [], [] -> Lwt.return 0
  | [], _ :: _ -> Lwt.return (-1)
  | _ :: _, [] -> Lwt.return 1
  | a1 :: l1, a2 :: l2 ->
    let%lwt c = cmp a1 a2 in
    if c <> 0 then Lwt.return c
    else compare_lwt cmp l1 l2

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

let cons_lwt h l =
  let%lwt l' = l in
  Lwt.return (h :: l')

let rec merge_lwt compare l1 l2 =
  match (l1, l2) with
  | [], l | l, [] ->
    Lwt.return l
  | h1 :: t1, h2 :: t2 ->
    let%lwt cmp = compare h1 h2 in
    if cmp <= 0 then cons_lwt h1 (merge_lwt compare t1 (h2 :: t2))
    else cons_lwt h2 (merge_lwt compare (h1 :: t1) t2)

(* Same as {!merge_lwt} but merges equal occurrences and counts them. *)
let rec merge_count_lwt compare l1 l2 =
  match (l1, l2) with
  | [], l | l, [] ->
    Lwt.return l
  | (h1, n1) :: t1, (h2, n2) :: t2 ->
    let%lwt cmp = compare h1 h2 in
    if cmp < 0 then cons_lwt (h1, n1) (merge_count_lwt compare t1 ((h2, n2) :: t2))
    else if cmp = 0 then cons_lwt (h1, n1 + n2) (merge_count_lwt compare t1 t2)
    else cons_lwt (h2, n2) (merge_count_lwt compare ((h1, n1) :: t1) t2)

(* morally, this could be called split, but the name is already taken *)
let rec untangle = function
  | [] -> [], []
  | [h] -> [h], []
  | h1 :: h2 :: t ->
    let l1, l2 = untangle t in
    h1 :: l1, h2 :: l2

let sort_lwt compare l =
  let rec sort_aux = function
    | [] -> Lwt.return []
    | [h] -> Lwt.return [h]
    | l ->
      let l1, l2 = untangle l in
      let%lwt l1s = sort_aux l1 in
      let%lwt l2s = sort_aux l2 in
      merge_lwt compare l1s l2s
  in
  sort_aux l

let sort_count_lwt compare l =
  let rec sort_aux = function
    | [] -> Lwt.return []
    | [h] -> Lwt.return [h]
    | l ->
      let l1, l2 = untangle l in
      let%lwt l1s = sort_aux l1 in
      let%lwt l2s = sort_aux l2 in
      merge_count_lwt compare l1s l2s
  in
  sort_aux (List.map (fun x -> (x, 1)) l)

let sort_uniq_lwt compare l =
  let%lwt l = sort_count_lwt compare l in
  Lwt.return (List.map fst l)
