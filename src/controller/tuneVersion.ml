open Dancelor_common
open Dancelor_model
open QueryHelpers
module Log = (val Log.create "dancelor.controller.tuneversion" : Logs.LOG)

let tune_version_from_query query =
  try
    let slug = query_string query "slug" in
    let tune = Tune.Database.get slug in
    try
      let subslug = query_string query "subslug" in
      (tune, Tune.version tune subslug)
    with
    | Error.Error _ ->
       (tune, Tune.default_version tune)
    | Not_found ->
       error "this version does not exist"
  with
    Not_found ->
    error "this tune does not exist"

let get query =
  tune_version_from_query query
  |> Tune.tune_version_to_jsonm
  |> Json.of_value
  |> Lwt.return

let get_ly query =
  let (_, version) = tune_version_from_query query in
  Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:(Tune.version_content version) ()

module Png = struct
  let cache : (Tune.t * Tune.version, string Lwt.t) Hashtbl.t = Hashtbl.create 8

  let template =
    let path = Filename.concat_l [Config.share; "lilypond"; "tune.ly"] in
    Log.debug (fun m -> m "Loading template file %s" path);
    let ichan =  open_in path in
    let template = Lexing.from_channel ichan |> Mustache.parse_lx in
    close_in ichan;
    Log.debug (fun m -> m "Loaded successfully");
    template

  let (>>=) = Lwt.bind

  let get query =
    let (tune, version) = tune_version_from_query query in
    let processor =
      try
        let processor = Hashtbl.find cache (tune, version) in
        Log.debug (fun m -> m "Found in cache");
        processor
      with
        Not_found ->
        let processor =
          Log.debug (fun m -> m "Not in the cache. Rendering the Lilypond version");
          let json = Json.of_value (Tune.tune_version_to_jsonm (tune, version)) in
          let lilypond = Mustache.render template json in
          let path = Filename.concat Config.cache "tune" in
          let fname_ly, fname_png =
            let fname = spf "%s-%x" (Tune.slug tune) (Random.int (1 lsl 29)) in
            (fname^".ly", fname^".png")
          in
          Lwt_io.with_file ~mode:Output (Filename.concat path fname_ly)
            (fun ochan -> Lwt_io.write ochan lilypond) >>= fun () ->
          Log.debug (fun m -> m "Processing with Lilypond");
          Lwt_process.with_process_full
            ~env:[|"PATH="^(Unix.getenv "PATH");
                   "LANG=en"|]
            (Lwt_process.shell
               ("cd " ^ path ^ " && " ^ Config.lilypond ^ " -dresolution=110 -dbackend=eps --loglevel=WARNING --png " ^ fname_ly))
            (fun process ->
              process#status >>= fun status ->
              (match status with
               | WEXITED 0 ->
                  Lwt.return ()
               | _ ->
                  Lwt_io.read process#stderr >>= fun output ->
                  Log.err (fun m -> m "Error while running Lilypond:@\n%a" pp_string_multiline output);
                  Lwt.return ())
            )
          >>= fun () ->
          Lwt.return (Filename.concat path fname_png)
        in
        Hashtbl.add cache (tune, version) processor;
        processor
    in
    processor >>= fun path_png ->
    Cohttp_lwt_unix.Server.respond_file ~fname:path_png ()
end
