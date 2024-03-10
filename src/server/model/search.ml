open Nes

module Score = Dancelor_common.Model.Score

let search
    ~cache
    ~values_getter
    ~scoring_function
    ~tiebreakers
    (* typically will be partially applied until here *)
    ?pagination
    ?(threshold=Float.min_float)
    filter
  =
  let%lwt results =
    Cache.use ~cache ~key:(threshold, filter) @@ fun () ->
    values_getter ()
    >>=| Score.lwt_map_from_list (scoring_function filter)
    >>=| (Score.list_filter_threshold threshold ||> Lwt.return)
    >>=| (Score.list_proj_sort_decreasing tiebreakers)
  in
  Lwt.return (
    List.length results,
    Option.fold ~none:Fun.id ~some:Dancelor_common.Model.Pagination.apply pagination results
  )
