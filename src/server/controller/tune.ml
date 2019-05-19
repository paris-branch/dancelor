open Nes
open Dancelor_server_model
open QueryHelpers
module Log = (val Dancelor_server_logs.create "controller.tune" : Logs.LOG)

let get tune _ =
  Dancelor_database.Tune.get tune

let get_ly tune _ =
  let%lwt tune = Dancelor_database.Tune.get tune in
  let%lwt body = Tune.content tune in
  Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body ()

let match_score needle haystack =
  let needle = Slug.from_string needle in
  let haystack = Slug.from_string haystack in
  1. -.
  if String.length needle = 0 then
    0.
  else
    let d = String.inclusion_distance needle haystack in
    (float_of_int d) /. (float_of_int (String.length needle))

let get_all : Tune.t Score.t list Controller.t = fun query ->
  Log.debug (fun m -> m "controller get_all");
  let%lwt name = query_string_opt query "name" in
  let%lwt author = query_string_opt query "author" in
  let%lwt kind =
    match%lwt query_string_opt query "kind" with
    | None -> Lwt.return_none
    | Some kind ->
      try
        Lwt.return_some (Kind.base_of_string kind)
      with
        Failure _ ->
        Lwt.fail Dancelor_common.Error.(Exn (BadQuery "kind must be 'j', 'p', 'r', 's' or 'w'"))
  in
  let%lwt keys =
    match%lwt query_string_opt query "keys" with
    | None -> Lwt.return_none
    | Some keys ->
      let keys = String.split_on_char ',' keys in
      Lwt.return_some (List.map Music.key_of_string keys)
  in
  let%lwt mode =
    match%lwt query_string_opt query "mode" with
    | None -> Lwt.return_none
    | Some "major" -> Lwt.return_some Music.Major
    | Some "minor" -> Lwt.return_some Music.Minor
    | Some other -> Lwt.fail Dancelor_common.Error.(Exn (BadQuery ("mode must be 'major' or 'minor', got " ^ other)))
  in
  let%lwt hard_limit = query_int ~or_:max_int query "hard-limit"  in
  let%lwt threshold = query_float ~or_:0. query "threshold" in
  let threshold = threshold /. 100. in
  let%lwt all = Dancelor_database.Tune.get_all () in
  let all = Score.list_from_values all in
  let%lwt all =
    Score.list_filter
      (match kind with
       | None ->
         fun _ -> Lwt.return_true
       | Some kind ->
         (fun tune ->
            let%lwt tune_group = Tune.group tune in
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
            let%lwt key' = Tune.key tune in
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
            let%lwt key' = Tune.key tune in
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
              let%lwt tune_group = Tune.group tune in
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
            let%lwt tune_group = Tune.group tune in
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
      (fun tune1 tune2 -> compare (Tune.slug tune1) (Tune.slug tune2))
      all
  in
  let all = List.sub hard_limit all in
  Lwt.return all (* FIXME: * 100 ? *)

module Png = struct
  let cache : (Tune.t, string Lwt.t) Cache.t = Cache.create ()

  (* let template =
    let path = Filename.concat_l [!Dancelor_server_config.share; "lilypond"; "tune.ly"] in
    Log.debug (fun m -> m "Loading template file %s" path);
    let ichan =  open_in path in
    let template = Lexing.from_channel ichan |> Mustache.parse_lx in
    close_in ichan;
    Log.debug (fun m -> m "Loaded successfully");
    template *)

  let render tune =
    Cache.use
      cache tune
      (fun () ->
         Log.debug (fun m -> m "Rendering the Lilypond version");
         let _json = Tune.to_yojson tune in
         assert false (* FIXME *)
         (* let lilypond = Mustache.render template (Json.to_ezjsonm json) in
         let path = Filename.concat !Dancelor_server_config.cache "tune" in
         let fname_ly, fname_png =
           let fname = spf "%s-%x" (Tune.slug tune) (Random.int (1 lsl 29)) in
           (fname^".ly", fname^".png")
         in
         Lwt_io.with_file ~mode:Output (Filename.concat path fname_ly)
           (fun ochan -> Lwt_io.write ochan lilypond) >>= fun () ->
         Log.debug (fun m -> m "Processing with Lilypond");
         Lilypond.run ~exec_path:path ~options:["-dresolution=110"; "-dbackend=eps"; "--png"] fname_ly
         >>= fun () ->
            Lwt.return (Filename.concat path fname_png)) *))

  let get tune _ =
    let%lwt tune = Dancelor_database.Tune.get tune in
    let%lwt path_png = render tune in
    Cohttp_lwt_unix.Server.respond_file ~fname:path_png ()
end
