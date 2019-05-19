open Nes
include Dancelor_common_model.Tune

let group t =
  let%lwt group = group t in
  Dancelor_server_database.TuneGroup.get group

let arranger t =
  match%lwt arranger t with
  | None -> Lwt.return_none
  | Some arranger ->
    let%lwt arranger = Dancelor_server_database.Credit.get arranger in
    Lwt.return_some arranger

(* * *)

let get = Dancelor_server_database.Tune.get

let match_score needle haystack =
  let needle = Slug.from_string needle in
  let haystack = Slug.from_string haystack in
  1. -.
  if String.length needle = 0 then
    0.
  else
    let d = String.inclusion_distance needle haystack in
    (float_of_int d) /. (float_of_int (String.length needle))

let get_all ?kind ?keys ?mode ?name ?author ?(threshold=0.) ?(hard_limit=max_int) () =
  let%lwt all = Dancelor_server_database.Tune.get_all () in
  let all = Score.list_from_values all in
  let%lwt all =
    Score.list_filter
      (match kind with
       | None ->
         fun _ -> Lwt.return_true
       | Some kind ->
         (fun tune ->
            let%lwt tune_group = group tune in
            let%lwt kind' = TuneGroup.kind tune_group in
            Lwt.return (kind' = kind)))
      all
  in
  let%lwt all =
    Score.list_filter
      (match keys with
       | None ->
         fun _ -> Lwt.return_true
       | Some keys ->
         (fun tune ->
            let%lwt key' = key tune in
            Lwt.return (List.mem key' keys)))
      all
  in
  let%lwt all =
    Score.list_filter
      (match mode with
       | None ->
         fun _ -> Lwt.return_true
       | Some mode ->
         (fun tune ->
            let%lwt key' = key tune in
            Lwt.return (snd key' = mode)))
      all
  in
  let%lwt all =
    Score.list_map_score
      (match name with
       | None ->
         (fun _ -> Lwt.return 1.)
       | Some name ->
         (fun tune ->
            let%lwt tune_name =
              let%lwt tune_group = group tune in
              TuneGroup.name tune_group
            in
            Lwt.return (match_score name tune_name)))
      all
  in
  let%lwt all =
    Score.list_map_score
      (match author with
       | None ->
         (fun _ -> Lwt.return 1.)
       | Some author ->
         (fun tune ->
            let%lwt tune_group = group tune in
            match%lwt TuneGroup.author tune_group with
            | None -> Lwt.return 0.
            | Some tune_author ->
              let%lwt tune_author = Credit.line tune_author in
              Lwt.return (match_score author tune_author)))
      all
  in
  let all =
    Score.list_filter_threshold threshold all
  in
  let all =
    Score.list_sort_decreasing
      (fun tune1 tune2 -> compare (slug tune1) (slug tune2))
      all
  in
  let all = List.sub hard_limit all in
  Lwt.return all (* FIXME: * 100 ? *)
