open NesList

(* FIXME: cannot test merge_count_lwt because it is not exported *)

(* let test_merge_lwt l1 l2 expected = *)
(*   assert *)
(*     (Lwt_main.run *)
(*        (let%lwt result = merge_lwt NesInt.compare_lwt l1 l2 in *)
(*         Lwt.return (result = expected))) *)

(* let () = test_merge_lwt [] [] [] *)
(* let () = test_merge_lwt [2; 3] [] [2; 3] *)
(* let () = test_merge_lwt [] [5; 8] [5; 8] *)
(* let () = test_merge_lwt [0; 1; 3; 5; 7] [0; 4; 5; 6] *)
(*     [0; 0; 1; 3; 4; 5; 5; 6; 7] *)

(* let test_merge_count_lwt l1 l2 expected = *)
(*   assert *)
(*     (Lwt_main.run *)
(*        (let%lwt result = merge_count_lwt NesInt.compare_lwt l1 l2 in *)
(*         Lwt.return (result = expected))) *)

(* let () = test_merge_count_lwt [] [] [] *)
(* let () = test_merge_count_lwt [(2, 4); (3, 5)] [] [(2, 4); (3, 5)] *)
(* let () = test_merge_count_lwt [] [(5, 4); (8, 5)] [(5, 4); (8, 5)] *)
(* let () = test_merge_count_lwt *)
(*     [(0, 4); (1, 7); (3, 8); (5, 9); (7, 10)] *)
(*     [(0, 2); (4, 4); (5, 3); (6, 6)] *)
(*     [(0, 6); (1, 7); (3, 8); (4, 4); (5, 12); (6, 6); (7, 10)] *)

let rec test_sort_lwt ~repeat ~length =
  if repeat = 0 then ()
  else
    let input = init length (fun _ -> Random.bits ()) in
    let output = Lwt_main.run (sort_lwt NesInt.compare_lwt input) in
    let expected = sort NesInt.compare input in
    assert (output = expected);
    test_sort_lwt ~repeat:(repeat-1) ~length

let () = test_sort_lwt ~repeat:1 ~length:0
let () = test_sort_lwt ~repeat:1 ~length:1
let () = test_sort_lwt ~repeat:4 ~length:2
let () = test_sort_lwt ~repeat:9 ~length:3
let () = test_sort_lwt ~repeat:25 ~length:5
let () = test_sort_lwt ~repeat:1_000 ~length:10
let () = test_sort_lwt ~repeat:1_000 ~length:100
let () = test_sort_lwt ~repeat:1_000 ~length:1_000
let () = test_sort_lwt ~repeat:1_000 ~length:10_000

let rec test_sort_count_lwt ~repeat ~length =
  if repeat = 0 then ()
  else
    let input = init length (fun _ -> Random.bits ()) in
    let output = Lwt_main.run (sort_count_lwt NesInt.compare_lwt input) in
    let expected = sort_count NesInt.compare input in
    assert (output = expected);
    test_sort_count_lwt ~repeat:(repeat-1) ~length

let () = test_sort_count_lwt ~repeat:1 ~length:0
let () = test_sort_count_lwt ~repeat:1 ~length:1
let () = test_sort_count_lwt ~repeat:4 ~length:2
let () = test_sort_count_lwt ~repeat:9 ~length:3
let () = test_sort_count_lwt ~repeat:25 ~length:5
let () = test_sort_count_lwt ~repeat:1_000 ~length:10
let () = test_sort_count_lwt ~repeat:1_000 ~length:100
let () = test_sort_count_lwt ~repeat:1_000 ~length:1_000
let () = test_sort_count_lwt ~repeat:1_000 ~length:10_000

let rec test_sort_uniq_lwt ~repeat ~length =
  if repeat = 0 then ()
  else
    let input = init length (fun _ -> Random.bits ()) in
    let output = Lwt_main.run (sort_uniq_lwt NesInt.compare_lwt input) in
    let expected = sort_uniq NesInt.compare input in
    assert (output = expected);
    test_sort_uniq_lwt ~repeat:(repeat-1) ~length

let () = test_sort_uniq_lwt ~repeat:1 ~length:0
let () = test_sort_uniq_lwt ~repeat:1 ~length:1
let () = test_sort_uniq_lwt ~repeat:4 ~length:2
let () = test_sort_uniq_lwt ~repeat:9 ~length:3
let () = test_sort_uniq_lwt ~repeat:25 ~length:5
let () = test_sort_uniq_lwt ~repeat:1_000 ~length:10
let () = test_sort_uniq_lwt ~repeat:1_000 ~length:100
let () = test_sort_uniq_lwt ~repeat:1_000 ~length:1_000
let () = test_sort_uniq_lwt ~repeat:1_000 ~length:10_000
