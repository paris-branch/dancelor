include Dancelor_common_model.Pagination

let apply p all =
  let rec apply start end_ = function
    | [] -> []
    | _ :: q when start > 0 -> apply (start - 1) (end_ - 1) q
    | h :: q when end_ > 0 -> h :: apply 0 (end_ - 1) q
    | _ -> []
  in
  apply (start p) (end_ p) all
  |> Lwt.return
