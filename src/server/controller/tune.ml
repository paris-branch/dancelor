open Nes
open Dancelor_server_model
module Log = (val Dancelor_server_logs.create "controller.tune" : Logs.LOG)

let get_ly tune _ =
  let%lwt tune = Tune.get tune in
  let%lwt body = Tune.content tune in
  Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body ()

let prepare_ly_file ?(show_meta=false) ?(meta_in_title=false) ~fname tune =
  let%lwt group = Tune.group tune in
  let%lwt name = TuneGroup.name group in
  let%lwt author =
    match%lwt TuneGroup.author group with
      | None -> Lwt.return ""
      | Some author -> Credit.line author
  in
  let title, piece =
    if show_meta then
      if meta_in_title then name, " " else "", name
    else
      "", ""
  in
  let subtitle, opus =
    if show_meta then
      if meta_in_title then author, " " else "", author
    else
      "", ""
  in
  let%lwt content = Tune.content tune in
  let lilypond =
    Format.with_formatter_to_string @@ fun fmt ->
    fpf fmt [%blob "template/version.ly"];
    fpf fmt [%blob "template/layout.ly"];
    fpf fmt [%blob "template/paper.ly"];
    fpf fmt [%blob "template/bar-numbering/beginning-of-line.ly"];
    fpf fmt [%blob "template/bar-numbering/partial-aware.ly"];
    fpf fmt [%blob "template/bar-numbering/repeat-aware.ly"];
    fpf fmt [%blob "template/repeat-volta-fancy.ly"];
    fpf fmt [%blob "template/header.ly"] title subtitle;
    fpf fmt [%blob "template/tune/header.ly"];
    fpf fmt [%blob "template/tune.ly"] piece opus content
  in
  Lwt_io.with_file ~mode:Output fname
    (fun ochan -> Lwt_io.write ochan lilypond)

module Svg = struct
  let cache : (Tune.t, string Lwt.t) Cache.t = Cache.create ()

  let render tune =
    Cache.use
      cache tune
      (fun () ->
         Log.debug (fun m -> m "Rendering the LilyPond version");
         let%lwt (fname_ly, fname_svg) =
           let%lwt slug = Tune.slug tune in
           let fname = spf "%s-%x" slug (Random.int (1 lsl 29)) in
           Lwt.return (fname^".ly", fname^".cropped.svg")
         in
         let path = Filename.concat !Dancelor_server_config.cache "tune" in
         prepare_ly_file ~show_meta:false ~fname:(Filename.concat path fname_ly) tune; %lwt
         Log.debug (fun m -> m "Processing with LilyPond");
         LilyPond.cropped_svg ~exec_path:path fname_ly; %lwt
         Lwt.return (Filename.concat path fname_svg))

  let get tune _ =
    Log.debug (fun m -> m "Tune.Svg.get %s" tune);
    let%lwt tune = Tune.get tune in
    let%lwt path_svg = render tune in
    Cohttp_lwt_unix.Server.respond_file ~fname:path_svg ()
end

module Pdf = struct
  let render tune =
    let%lwt (fname_ly, fname_pdf) =
      let%lwt slug = Tune.slug tune in
      let fname = spf "%s-%x-with-meta" slug (Random.int (1 lsl 29)) in
      Lwt.return (fname^".ly", fname^".pdf")
    in
    let path = Filename.concat !Dancelor_server_config.cache "tune" in
    prepare_ly_file ~show_meta:true ~meta_in_title:true
      ~fname:(Filename.concat path fname_ly) tune; %lwt
      Log.debug (fun m -> m "Processing with LilyPond");
    LilyPond.run ~exec_path:path fname_ly; %lwt
    Lwt.return (Filename.concat path fname_pdf)

  let get tune _ =
    Log.debug (fun m -> m "Tune.pdf.get %s" tune);
    let%lwt tune = Tune.get tune in
    let%lwt path_pdf = render tune in
    Cohttp_lwt_unix.Server.respond_file ~fname:path_pdf ()
end
