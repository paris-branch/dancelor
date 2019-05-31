open Nes
open Dancelor_server_model
open QueryHelpers
module Log = (val Dancelor_server_logs.create "controller.tune" : Logs.LOG)

let get tune _ =
  Tune.get tune

let get_ly tune _ =
  let%lwt tune = Tune.get tune in
  let%lwt body = Tune.content tune in
  Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body ()

let all query =
  let%lwt filter =
    match%lwt query_string_opt query "filter" with
    | None -> Lwt.return_none
    | Some filter ->
      match TuneFilter.of_yojson (Yojson.Safe.from_string filter) with
      | Ok filter -> Lwt.return_some filter
      | Error msg -> Lwt.fail Dancelor_common.Error.(Exn (BadQuery msg))
  in
  let%lwt pagination =
    match%lwt query_string_opt query "pagination" with
    | None -> Lwt.return_none
    | Some pagination ->
      match Pagination.of_yojson (Yojson.Safe.from_string pagination) with
      | Ok pagination -> Lwt.return_some pagination
      | Error msg -> Lwt.fail Dancelor_common.Error.(Exn (BadQuery msg))
  in
  Tune.all ?filter ?pagination ()

let search query =
  let%lwt filter =
    match%lwt query_string_opt query "filter" with
    | None -> Lwt.return_none
    | Some filter ->
      match TuneFilter.of_yojson (Yojson.Safe.from_string filter) with
      | Ok filter -> Lwt.return_some filter
      | Error msg -> Lwt.fail Dancelor_common.Error.(Exn (BadQuery msg))
  in
  let%lwt pagination =
    match%lwt query_string_opt query "pagination" with
    | None -> Lwt.return_none
    | Some pagination ->
      match Pagination.of_yojson (Yojson.Safe.from_string pagination) with
      | Ok pagination -> Lwt.return_some pagination
      | Error msg -> Lwt.fail Dancelor_common.Error.(Exn (BadQuery msg))
  in
  let%lwt threshold = query_float_opt query "threshold" in
  let%lwt query = query_strings query "query" in
  Tune.search ?filter ?pagination ?threshold query

module Png = struct
  let cache : (Tune.t, string Lwt.t) Cache.t = Cache.create ()

  let render tune =
    Cache.use
      cache tune
      (fun () ->
         Log.debug (fun m -> m "Rendering the Lilypond version");
         let%lwt content = Tune.content tune in
         let lilypond =
           Format.sprintf
             [%blob "template/tune.ly"]
             content
         in
         let path = Filename.concat !Dancelor_server_config.cache "tune" in
         let%lwt (fname_ly, fname_png) =
           let%lwt slug = Tune.slug tune in
           let fname = spf "%s-%x" slug (Random.int (1 lsl 29)) in
           Lwt.return (fname^".ly", fname^".png")
         in
         let%lwt () =
           Lwt_io.with_file ~mode:Output (Filename.concat path fname_ly)
             (fun ochan -> Lwt_io.write ochan lilypond)
         in
         Log.debug (fun m -> m "Processing with Lilypond");
         let%lwt () =
           Lilypond.run
             ~exec_path:path
             ~options:["-dresolution=110"; "-dbackend=eps"; "--png"]
             fname_ly
         in
         Lwt.return (Filename.concat path fname_png))

  let get tune _ =
    let%lwt tune = Tune.get tune in
    let%lwt path_png = render tune in
    Cohttp_lwt_unix.Server.respond_file ~fname:path_png ()
end