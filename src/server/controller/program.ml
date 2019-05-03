open Nes
open Dancelor_server_model
open QueryHelpers
module Log = (val Dancelor_server_logs.create "controller.program" : Logs.LOG)

let get program _ =
  program
  |> Dancelor_database.Program.get
  |> Program.to_jsonm
  |> (fun json -> Lwt.return (`O ["program", json]))

let get_all query =
  let contains_set =
    match query_string_opt query "contains" with
    | None -> (fun _ -> true)
    | Some set -> Program.contains set
  in
  Dancelor_database.Program.get_all ()
  |> Seq.filter contains_set
  |> Seq.map Program.to_jsonm
  |> List.of_seq
  |> (fun json -> Lwt.return (`O ["programs", `A json]))

module Ly = struct
  let template =
    let path = Filename.concat_l [!Dancelor_server_config.share; "lilypond"; "program.ly"] in
    Log.debug (fun m -> m "Loading template file %s" path);
    let ichan = open_in path in
    let template = Lexing.from_channel ichan |> Mustache.parse_lx in
    close_in ichan;
    Log.debug (fun m -> m "Loaded successfully");
    template

  let render ?transpose_target program =
    Program.to_json program
    |> Json.add_field "transpose"
         (match transpose_target with
          | None -> `Bool false
          | Some target ->
             let instrument =
               match target with
               | "bes," -> "B flat"
               | "ees" -> "E flat"
               | _ -> target
             in
             `O [ "target", `String target ;
                  "instrument", `String instrument ])
    |> Json.to_ezjsonm
    |> Mustache.render template
end

module Pdf = struct
  let cache : (Program.t, string Lwt.t) Cache.t = Cache.create ()

  let (>>=) = Lwt.bind

  let render ?transpose_target program =
    Cache.use
      cache program
      (fun () ->
        let lilypond = Ly.render ?transpose_target program in
        let path = Filename.concat !Dancelor_server_config.cache "program" in
        let fname_ly, fname_pdf =
          let fname = spf "%s-%x" (Program.slug program) (Random.int (1 lsl 29)) in
          (fname^".ly", fname^".pdf")
        in
        Lwt_io.with_file ~mode:Output (Filename.concat path fname_ly)
          (fun ochan -> Lwt_io.write ochan lilypond) >>= fun () ->
        Log.debug (fun m -> m "Processing with Lilypond");
        Lilypond.run ~exec_path:path fname_ly >>= fun () ->
        let path_pdf = Filename.concat path fname_pdf in
        Lwt.return path_pdf)

  let get program query =
    let program = Dancelor_database.Program.get program in
    render ?transpose_target:(query_string_opt query "transpose-target") program >>= fun path_pdf ->
    Cohttp_lwt_unix.Server.respond_file ~fname:path_pdf ()
end
