include List

let rec map_filter f = function
  | [] -> []
  | h :: t ->
     match f h with
     | None -> map_filter f t
     | Some x -> x :: map_filter f t

let rec sub n l =
  if n <= 0 then []
  else begin
    match l with
    | [] -> []
    | h::t -> h::(sub (n-1) t)
  end

let hd_opt = function
  | [] -> None
  | h :: _ -> Some h

let bd l =
  let rec bd acc = function
    | [] -> failwith "bd"
    | [_] -> List.rev acc
    | h::q -> bd (h::acc) q
  in
  bd [] l

let rec ft = function
  | [] -> failwith "ft"
  | [e] -> e
  | _::q -> ft q

let intertwine f l =
  let rec intertwine i = function
    | [] -> []
    | [e] -> [e]
    | h :: q ->
      h :: (f i) :: intertwine (i+1) q
  in
  intertwine 0 l

let rec compare_lwt cmp l1 l2 =
  match l1, l2 with
  | [], [] -> Lwt.return 0
  | [], _::_ -> Lwt.return (-1)
  | _::_, [] -> Lwt.return 1
  | a1::l1, a2::l2 ->
    let%lwt c = cmp a1 a2 in
    if c <> 0 then Lwt.return c
    else compare_lwt cmp l1 l2

let sort_count cmp l =
  (* count_duplicates assumes that the list is sorted *)
  let rec count_duplicates acc (prev, nb) = function
    | [] -> rev ((prev, nb) :: acc)
    | h :: t ->
      if cmp h prev = 0 then
        count_duplicates acc (prev, nb+1) t
      else
        count_duplicates ((prev,nb)::acc) (h,1) t
  in
  match sort cmp l with
  | [] -> []
  | h :: t -> count_duplicates [] (h,1) t

(******************************************************************************)
(* Lwt-Aware Sorting functions *)

let cons_lwt h l =
  let%lwt l' = l in
  Lwt.return (h::l')

let rec merge_lwt compare l1 l2 =
  match (l1, l2) with
  | [], l | l, [] ->
    Lwt.return l
  | h1::t1, h2::t2 ->
    let%lwt cmp = compare h1 h2 in
    if cmp <= 0 then cons_lwt h1 (merge_lwt compare t1 (h2::t2))
    else cons_lwt h2 (merge_lwt compare (h1::t1) t2)

let%test_module _ = (module struct
  let test_merge_lwt l1 l2 expected =
    Lwt_main.run
      (let%lwt result = merge_lwt NesInt.compare_lwt l1 l2 in
       Lwt.return (result = expected))

  let%test _ = test_merge_lwt [] [] []
  let%test _ = test_merge_lwt [2; 3] [] [2; 3]
  let%test _ = test_merge_lwt [] [5; 8] [5; 8]
  let%test _ = test_merge_lwt [0; 1; 3; 5; 7] [0; 4; 5; 6]
      [0; 0; 1; 3; 4; 5; 5; 6; 7]
end)

(* Same as {!merge_lwt} but merges equal occurrences and counts them. *)
let rec merge_count_lwt compare l1 l2 =
  match (l1, l2) with
  | [], l | l, [] ->
    Lwt.return l
  | (h1,n1) :: t1, (h2,n2) :: t2 ->
    let%lwt cmp = compare h1 h2 in
    if cmp < 0 then cons_lwt (h1,n1) (merge_count_lwt compare t1 ((h2,n2) :: t2))
    else if cmp = 0 then cons_lwt (h1,n1+n2) (merge_count_lwt compare t1 t2)
    else cons_lwt (h2,n2) (merge_count_lwt compare ((h1,n1) :: t1) t2)

let%test_module _ = (module struct
  let test_merge_count_lwt l1 l2 expected =
    Lwt_main.run
      (let%lwt result = merge_count_lwt NesInt.compare_lwt l1 l2 in
       Lwt.return (result = expected))

  let%test _ = test_merge_count_lwt [] [] []
  let%test _ = test_merge_count_lwt [(2, 4); (3, 5)] [] [(2, 4); (3, 5)]
  let%test _ = test_merge_count_lwt [] [(5, 4); (8, 5)] [(5, 4); (8, 5)]
  let%test _ = test_merge_count_lwt
      [(0, 4); (1, 7); (3, 8); (5, 9); (7, 10)]
      [(0, 2); (4, 4); (5, 3); (6, 6)]
      [(0, 6); (1, 7); (3, 8); (4, 4); (5, 12); (6, 6); (7, 10)]
end)

(* morally, this could be called split, but the name is already taken *)
let rec untangle = function
  | [] -> [], []
  | [h] -> [h],[]
  | h1::h2::t ->
    let l1,l2 = untangle t in
    h1::l1, h2::l2

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
