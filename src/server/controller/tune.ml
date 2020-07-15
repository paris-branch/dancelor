open Nes
open Dancelor_server_model
module Log = (val Dancelor_server_logs.create "controller.tune" : Logs.LOG)

let get_ly tune _ =
  let%lwt tune = Tune.get tune in
  let%lwt body = Tune.content tune in
  Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body ()

module Svg = struct
  let cache : (Tune.t, string Lwt.t) Cache.t = Cache.create ()

  let render tune =
    Cache.use
      cache tune
      (fun () ->
         Log.debug (fun m -> m "Rendering the Lilypond version");
         let%lwt content = Tune.content tune in
         let lilypond =
           Format.with_formatter_to_string @@ fun fmt ->
           fpf fmt [%blob "template/version.ly"];
           fpf fmt [%blob "template/paper.ly"];
           fpf fmt [%blob "template/layout.ly"];
           fpf fmt [%blob "template/tune/header.ly"];
           fpf fmt [%blob "template/tune.ly"] content
         in
         let path = Filename.concat !Dancelor_server_config.cache "tune" in
         let%lwt (fname_ly, fname_svg) =
           let%lwt slug = Tune.slug tune in
           let fname = spf "%s-%x" slug (Random.int (1 lsl 29)) in
           Lwt.return (fname^".ly", fname^".cropped.svg")
         in
         Lwt_io.with_file ~mode:Output (Filename.concat path fname_ly)
           (fun ochan -> Lwt_io.write ochan lilypond); %lwt
         Log.debug (fun m -> m "Processing with Lilypond");
         Lilypond.cropped_svg ~exec_path:path fname_ly; %lwt
         Lwt.return (Filename.concat path fname_svg))

  let get tune _ =
    Log.debug (fun m -> m "Tune.Svg.get %s" tune);
    let%lwt tune = Tune.get tune in
    let%lwt path_svg = render tune in
    Cohttp_lwt_unix.Server.respond_file ~fname:path_svg ()
end
