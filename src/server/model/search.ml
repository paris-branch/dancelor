open Nes

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
  (* We cache the computation of the results but not the computation of the
     slice because that really isn't the expensive part. *)
  let%lwt results =
    Cache.use ~cache ~key:(threshold, filter) @@ fun () ->
    let%lwt values = values_getter () in
    (* For each value, compute its score and return the pair (value, score). *)
    let%lwt values = Lwt_list.map_s (fun value -> Lwt.map (pair value) (scoring_function filter value)) values in
    (* Keep only values whose score is above the given threshold. *)
    let values = List.filter (fun value -> snd value >= threshold) values in
    (* Sort by score, decreasing, falling back on the tiebreakers otherwise. *)
    let comparisons =
      Lwt_list.decreasing (Lwt.return % snd) Float.compare
      :: List.map (fun compare -> fun x y -> compare (fst x) (fst y)) tiebreakers
    in
    let%lwt values = Lwt_list.sort_multiple comparisons values in
    (* Remove the scores again. *)
    Lwt.return @@ List.map fst values
  in
  (* Return the pair of the total number of results and the requested slice. *)
  Lwt.return (
    List.length results,
    Option.fold ~none:Fun.id ~some:Dancelor_common.Model.Pagination.apply pagination results
  )
