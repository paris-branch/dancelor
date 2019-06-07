let proj_sort proj compare l =
  let%lwt l_w_proj =
    Lwt_list.map_s
      (fun elt -> Lwt.bind (proj elt) (fun p -> Lwt.return (elt, p))) l
  in
  List.sort (fun (_, p1) (_, p2) -> compare p1 p2) l_w_proj
  |> List.map fst
  |> Lwt.return
