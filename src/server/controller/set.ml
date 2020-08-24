open Nes
open Dancelor_server_model
open QueryHelpers
module Log = (val Dancelor_server_logs.create "controller.set" : Logs.LOG)

module Ly = struct

  let render ?transpose_target set =
    let (transpose, target, instrument) =
      (* transpose introduces a comment when we don't want to transpose *)
      match transpose_target with
      | None -> ("%", "c", "C")
      | Some target ->
        let instrument =
          match target with
          | "bes," -> "B flat"
          | "ees" -> "E flat"
          | _ -> target
        in
        ("", target, instrument)
    in
    let (res, prom) =
      Format.with_formatter_to_string_gen @@ fun fmt ->
      let%lwt title = Set.name set in
      let%lwt kind = Set.kind set in
      let%lwt tunes = Set.tunes set in
      fpf fmt [%blob "template/version.ly"];
      fpf fmt [%blob "template/layout.ly"];
      fpf fmt [%blob "template/paper.ly"];
      fpf fmt [%blob "template/set/paper.ly"];
      fpf fmt [%blob "template/bar-numbering/repeat-aware.ly"];
      fpf fmt [%blob "template/bar-numbering/bar-number-in-instrument-name-engraver.ly"];
      fpf fmt [%blob "template/bar-numbering/beginning-of-line.ly"];
      fpf fmt [%blob "template/repeat-volta-fancy.ly"];
      fpf fmt [%blob "template/set/header.ly"]
        title (Kind.dance_to_string kind)
        transpose instrument;
      Lwt_list.iter_s
        (fun tune ->
           let%lwt content = Tune.content tune in
           let%lwt group = Tune.group tune in
           let%lwt name = TuneGroup.name group in
           let%lwt author =
             match%lwt TuneGroup.author group with
             | None -> Lwt.return ""
             | Some author -> Credit.line author
           in
           fpf fmt [%blob "template/set/tune.ly"]
             name author
             transpose target
             content
             transpose;
           Lwt.return ())
        tunes
    in
    prom; %lwt
    Lwt.return res

  let get set query =
    let%lwt set = Set.get set in
    let%lwt transpose_target = query_string_opt query "transpose-target" in
    let%lwt lilypond = render ?transpose_target set in
    Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:lilypond ()
end

module Pdf = struct
  let cache : (('a * Set.t), string Lwt.t) Cache.t = Cache.create ()

  let render ?transpose_target set =
    Cache.use
      cache (transpose_target, set)
      (fun () ->
        let%lwt lilypond = Ly.render ?transpose_target set in
        let path = Filename.concat !Dancelor_server_config.cache "set" in
        let%lwt (fname_ly, fname_pdf) =
          let%lwt slug = Set.slug set in
          let fname = spf "%s-%x" slug (Random.int (1 lsl 29)) in
          Lwt.return (fname^".ly", fname^".pdf")
        in
        Lwt_io.with_file ~mode:Output (Filename.concat path fname_ly)
          (fun ochan -> Lwt_io.write ochan lilypond); %lwt
        Log.debug (fun m -> m "Processing with LilyPond");
        LilyPond.run ~exec_path:path fname_ly; %lwt
        let path_pdf = Filename.concat path fname_pdf in
        Lwt.return path_pdf)

  let get set query =
    let%lwt set = Set.get set in
    let%lwt transpose_target = query_string_opt query "transpose-target" in
    let%lwt path_pdf = render ?transpose_target set in
    Cohttp_lwt_unix.Server.respond_file ~fname:path_pdf ()
end
