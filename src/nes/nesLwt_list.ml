include Lwt_list

(* The following is copied from the standard library; except cmp is Lwt. *)

let stable_sort cmp l =

  (* Compatibility layer to make code as similar to that of stdlib as possible. *)
  let gt = (>) in
  let le = (<=) in
  let cmp e1 e2 op c =
    let%lwt c0 = cmp e1 e2 in
    Lwt.return (op c0 c)
  in
  let rec rev_merge l1 l2 accu =
    match l1, l2 with
    | [], l2 -> Lwt.return (List.rev_append l2 accu)
    | l1, [] -> Lwt.return (List.rev_append l1 accu)
    | h1 :: t1, h2 :: t2 ->
      if%lwt cmp h1 h2 le 0 then rev_merge t1 l2 (h1 :: accu)
      else rev_merge l1 t2 (h2 :: accu)
  in
  let rec rev_merge_rev l1 l2 accu =
    match l1, l2 with
    | [], l2 -> Lwt.return (List.rev_append l2 accu)
    | l1, [] -> Lwt.return (List.rev_append l1 accu)
    | h1 :: t1, h2 :: t2 ->
      if%lwt cmp h1 h2 gt 0 then rev_merge_rev t1 l2 (h1 :: accu)
      else rev_merge_rev l1 t2 (h2 :: accu)
  in
  let rec sort n l =
    match n, l with
    | 2, x1 :: x2 :: tl ->
      let%lwt s =
        if%lwt cmp x1 x2 le 0 then Lwt.return [x1; x2]
        else Lwt.return [x2; x1]
      in
      Lwt.return (s, tl)
    | 3, x1 :: x2 :: x3 :: tl ->
      let%lwt s =
        if%lwt cmp x1 x2 le 0 then
          if%lwt cmp x2 x3 le 0 then Lwt.return [x1; x2; x3]
          else if%lwt cmp x1 x3 le 0 then Lwt.return [x1; x3; x2]
          else Lwt.return [x3; x1; x2]
        else if%lwt cmp x1 x3 le 0 then Lwt.return [x2; x1; x3]
        else if%lwt cmp x2 x3 le 0 then Lwt.return [x2; x3; x1]
        else Lwt.return [x3; x2; x1]
      in
      Lwt.return (s, tl)
    | n, l ->
      let n1 = n asr 1 in
      let n2 = n - n1 in
      let%lwt (s1, l2) = rev_sort n1 l in
      let%lwt (s2, tl) = rev_sort n2 l2 in
      let%lwt s = rev_merge_rev s1 s2 [] in
      Lwt.return (s, tl)

  and rev_sort n l =
    match n, l with
    | 2, x1 :: x2 :: tl ->
      let%lwt s =
        if%lwt cmp x1 x2 gt 0 then Lwt.return [x1; x2]
        else Lwt.return [x2; x1]
      in
      Lwt.return (s, tl)
    | 3, x1 :: x2 :: x3 :: tl ->
      let%lwt s =
        if%lwt cmp x1 x2 gt 0 then
          if%lwt cmp x2 x3 gt 0 then Lwt.return [x1; x2; x3]
          else if%lwt cmp x1 x3 gt 0 then Lwt.return [x1; x3; x2]
          else Lwt.return [x3; x1; x2]
        else if%lwt cmp x1 x3 gt 0 then Lwt.return [x2; x1; x3]
        else if%lwt cmp x2 x3 gt 0 then Lwt.return [x2; x3; x1]
        else Lwt.return [x3; x2; x1]
      in
      Lwt.return (s, tl)
    | n, l ->
      let n1 = n asr 1 in
      let n2 = n - n1 in
      let%lwt (s1, l2) = sort n1 l in
      let%lwt (s2, tl) = sort n2 l2 in
      let%lwt s = rev_merge s1 s2 [] in
      Lwt.return (s, tl)
  in
  let len = List.length l in
  if len < 2 then Lwt.return l
  else
    let%lwt l = sort len l in
    Lwt.return (fst l)

let sort = stable_sort

(* Projection sort *)

let rec compare_multiple compares x y =
  match compares with
  | [] -> Lwt.return 0
  | compare :: compares ->
    let%lwt c = compare x y in
    if c <> 0 then Lwt.return c else compare_multiple compares x y

let increasing proj comp x y =
  let%lwt x = proj x in
  let%lwt y = proj y in
  Lwt.return (comp x y)

let decreasing proj comp x y = increasing proj comp y x

let sort_multiple compares l =
  sort (compare_multiple compares) l
