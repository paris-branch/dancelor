open Nes
open Dancelor_server_model
open QueryHelpers
module Log = (val Dancelor_server_logs.create "controller.program" : Logs.LOG)

module Ly = struct
  let render ?transpose_target program =
    let (target, instrument) =
      match transpose_target with
      | None -> ("c", "C")
      | Some target ->
        let instrument =
          match target with
          | "bes," -> "B flat"
          | "ees" -> "E flat"
          | _ -> target
        in
        (target, instrument)
    in
    let (res, prom) =
      Format.with_formatter_to_string_gen @@ fun fmt ->
      let%lwt name = Program.name program in
      fpf fmt [%blob "template/version.ly"];
      fpf fmt [%blob "template/program/macros.ly"];
      fpf fmt [%blob "template/layout.ly"];
      fpf fmt [%blob "template/program/globals.ly"]
        name instrument;
      fpf fmt [%blob "template/paper.ly"];
      fpf fmt [%blob "template/program/paper.ly"];
      fpf fmt [%blob "template/bar-numbering/beginning-of-line.ly"];
      fpf fmt [%blob "template/bar-numbering/repeat-aware.ly"];
      fpf fmt [%blob "template/repeat-volta-fancy.ly"];
      fpf fmt [%blob "template/program/book_beginning.ly"];
      let%lwt () =
        let%lwt sets = Program.sets program in
        Lwt_list.iter_s
          (fun set ->
             let%lwt name = Set.name set in
             let%lwt kind = Set.kind set in
             let kind = Kind.dance_to_string kind in
             fpf fmt [%blob "template/program/set_beginning.ly"]
               name kind name kind;
             let%lwt () =
               let%lwt tunes = Set.tunes set in
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
                    fpf fmt [%blob "template/program/tune.ly"]
                      name author name target content;
                    Lwt.return ())
                 tunes
             in
             fpf fmt [%blob "template/program/set_end.ly"];
             Lwt.return ())
          sets
      in
      fpf fmt [%blob "template/program/book_end.ly"];
      Lwt.return ()
    in
    prom; %lwt
    Lwt.return res
end

module Pdf = struct
  let cache : ('a * Program.t, string Lwt.t) Cache.t = Cache.create ()

  let render ?transpose_target program =
    Cache.use
      cache (transpose_target, program)
      (fun () ->
        let%lwt lilypond = Ly.render ?transpose_target program in
        let path = Filename.concat !Dancelor_server_config.cache "program" in
        let%lwt (fname_ly, fname_pdf) =
          let%lwt slug = Program.slug program in
          let fname = spf "%s-%x" slug (Random.int (1 lsl 29)) in
          Lwt.return (fname^".ly", fname^".pdf")
        in
        Lwt_io.with_file ~mode:Output (Filename.concat path fname_ly)
          (fun ochan -> Lwt_io.write ochan lilypond); %lwt
        Log.debug (fun m -> m "Processing with LilyPond");
        LilyPond.run ~exec_path:path fname_ly; %lwt
        let path_pdf = Filename.concat path fname_pdf in
        Lwt.return path_pdf)

  let get program query =
    let%lwt program = Program.get program in
    let%lwt transpose_target = query_string_opt query "transpose-target" in
    let%lwt path_pdf = render ?transpose_target program in
    Cohttp_lwt_unix.Server.respond_file ~fname:path_pdf ()
end
