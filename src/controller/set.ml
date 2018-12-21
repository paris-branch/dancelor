open Dancelor_common
open Dancelor_model
open QueryHelpers
module Log = (val Log.create "dancelor.controller.set" : Logs.LOG)

let get query =
  let slug = query_string query "slug" in
  try
    Set.Database.get slug
    |> Set.to_jsonm
    |> (fun json -> Lwt.return (`O ["set", json]))
  with
    Not_found ->
    error "this set does not exist"

let get_all _query =
  Set.Database.get_all ()
  |> List.map Set.to_jsonm
  |> (fun json -> Lwt.return (`O ["sets", `A json]))

let template =
  let path = Filename.concat_l [Config.share; "lilypond"; "set.ly"] in
  Log.debug (fun m -> m "Loading template file %s" path);
  let ichan = open_in path in
  let template = Lexing.from_channel ichan |> Mustache.parse_lx in
  close_in ichan;
  Log.debug (fun m -> m "Loaded successfully");
  template

let get_pdf query =
  let (>>=) = Lwt.bind in
  try
    let slug = query_string query "slug" in
    let set = Set.Database.get slug in
    let json = Set.to_json set in
    let lilypond = Mustache.render template (Json.to_ezjsonm json) in
    let path = Filename.concat Config.cache "set" in
    let fname_ly, fname_pdf =
      let fname = spf "%s-%x" (Set.slug set) (Random.int (1 lsl 29)) in
      (fname^".ly", fname^".pdf")
    in
    Lwt_io.with_file ~mode:Output (Filename.concat path fname_ly)
      (fun ochan -> Lwt_io.write ochan lilypond) >>= fun () ->
    Log.debug (fun m -> m "Processing with Lilypond");
    Lwt_process.with_process_full
      ~env:[|"PATH="^(Unix.getenv "PATH");
             "LANG=en"|]
      (Lwt_process.shell
         ("cd " ^ path ^ " && " ^ Config.lilypond ^ " --loglevel=WARNING " ^ fname_ly))
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
    let path_pdf = Filename.concat path fname_pdf in
    Cohttp_lwt_unix.Server.respond_file ~fname:path_pdf ()
  with
    Not_found ->
    error "this set does not exist"

let save query =
  let name = query_string query "name" in
  let kind = Kind.dance_of_string (query_string query "kind") in
  let tunes =
    query_strings query "tunes"
    |> List.map Tune.Database.get_tune_version
  in
  Set.Database.create ~name ~kind ~tunes ()
  |> Set.to_jsonm
  |> (fun json -> Lwt.return (`O ["set", json]))
