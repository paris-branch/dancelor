open Nes

type 'a t =
  { score : float ;
    value : 'a }

let lwt_map_from_list score =
  Lwt_list.map_s
    (fun value ->
       let%lwt score = score value in
       Lwt.return { score; value })

let list_filter_threshold threshold =
  List.filter (fun score -> score.score >= threshold)

let list_proj_sort_decreasing compares l =
  let compares =
    List.map
      (fun compare ->
         fun x y -> compare x.value y.value)
      compares
  in
  let compares =
    (Lwt_list.decreasing (fun x -> Lwt.return x.score) compare)
    :: compares
  in
  Lwt_list.sort_multiple compares l

let list_erase l =
  List.map (fun score -> score.value) l

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
    >>=| lwt_map_from_list (scoring_function filter)
    >>=| (list_filter_threshold threshold ||> Lwt.return)
    >>=| (list_proj_sort_decreasing tiebreakers)
    >>=| (Lwt.return % list_erase)
  in
  Lwt.return (
    List.length results,
    Option.fold ~none:Fun.id ~some:Dancelor_common.Model.Pagination.apply pagination results
  )
