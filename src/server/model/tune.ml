open Nes
include Dancelor_common_model.Tune

let group = group >=>| TuneGroup.get
let arranger = arranger >=>?| (Credit.get >=>| Lwt.return_some)
let sources = sources >=>| Lwt_list.map_s Source.get

let content = Dancelor_server_database.Tune.read_content

(* * *)

let get = Dancelor_server_database.Tune.get

let () =
  Madge_server.(
    register ~endpoint:Endpoint.get @@ fun {a} _ ->
    get (a Arg.slug)
  )

(* FIXME: merge apply_filter and apply_filter_on_scores *)

let apply_filter filter all =
  let%lwt f_group_author = TuneFilter.group_author filter in
  let%lwt f_group_kind = TuneFilter.group_kind filter in
  let%lwt f_key = TuneFilter.key filter in
  let%lwt f_bars = TuneFilter.bars filter in
  Lwt_list.filter_s
    (fun tune ->
       let%lwt group = group tune in
       let%lwt group_author = TuneGroup.author group in
       let%lwt group_kind = TuneGroup.kind group in
       let%lwt key = key tune in
       let%lwt bars = bars tune in
       Lwt.return
         (
           (f_group_author = [] || (match group_author with None -> true | Some group_author -> List.mem group_author f_group_author))
           && (f_group_kind = [] || List.mem group_kind f_group_kind)
           && (f_key = [] || List.mem key f_key)
           && (f_bars = [] || List.mem bars f_bars)
         ))
    all

let apply_filter_on_scores filter all =
  let%lwt f_group_author = TuneFilter.group_author filter in
  let%lwt f_group_kind = TuneFilter.group_kind filter in
  let%lwt f_key = TuneFilter.key filter in
  let%lwt f_bars = TuneFilter.bars filter in
  Score.list_filter
    (fun tune ->
       let%lwt group = group tune in
       let%lwt group_author = TuneGroup.author group in
       let%lwt group_kind = TuneGroup.kind group in
       let%lwt key = key tune in
       let%lwt bars = bars tune in
       Lwt.return
         (
           (f_group_author = [] || (match group_author with None -> true | Some group_author -> List.mem group_author f_group_author))
           && (f_group_kind = [] || List.mem group_kind f_group_kind)
           && (f_key = [] || List.mem key f_key)
           && (f_bars = [] || List.mem bars f_bars)
         ))
    all

let all ?filter ?pagination () =
  Dancelor_server_database.Tune.get_all ()
  >>=| Option.unwrap_map_or Lwt.return apply_filter filter
  >>=| Lwt_list.proj_sort_s ~proj:slug compare (* FIXME: We shouldn't sort wrt. slugs. *)
  >>=| Option.unwrap_map_or Lwt.return Pagination.apply pagination

let () =
  Madge_server.(
    register ~endpoint:Endpoint.all @@ fun _ {o} ->
    all ?filter:(o Arg.filter) ?pagination:(o Arg.pagination) ()
  )

let search string tune =
  let%lwt group = group tune in
  let%lwt name = TuneGroup.name group in
  let%lwt alt_names = TuneGroup.alt_names group in
  List.map (String.sensible_inclusion_proximity ~needle:string) (name :: alt_names)
  |> List.fold_left max 0.
  |> Lwt.return

let search ?filter ?pagination ?(threshold=0.) string =
  Dancelor_server_database.Tune.get_all ()
  >>=| Score.lwt_map_from_list (search string)
  >>=| Option.unwrap_map_or Lwt.return apply_filter_on_scores filter
  >>=| (Score.list_filter_threshold threshold ||> Lwt.return)
  >>=| Score.list_proj_sort_decreasing ~proj:(group >=>| TuneGroup.name) String.sensible_compare
  >>=| Option.unwrap_map_or Lwt.return Pagination.apply pagination

let () =
  Madge_server.(
    register ~endpoint:Endpoint.search @@ fun {a} {o} ->
    search
      ?filter:    (o Arg.filter)
      ?pagination:(o Arg.pagination)
      ?threshold: (o Arg.threshold)
      (a Arg.string)
  )
