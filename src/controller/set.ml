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

let compose query =
  let open Option in
  let name = `String (value ~default:"" (query_string_opt query "name")) in
  let kind = `String (value ~default:"" (query_string_opt query "kind")) in
  let tune_jsons, error_jsons =
    match query_strings_opt query "tunes" with
    | Some tune_version_slugs ->
       List.map_partition
         (fun tune_version_slug ->
           try
             Tune.Database.get_tune_version tune_version_slug
             |> Tune.tune_version_to_jsonm
             |> (fun json -> List.A json)
           with
             Not_found -> List.B (`O ["message", `String ("tune \"" ^ tune_version_slug ^ "\" does not exist")]))
         tune_version_slugs
    | None ->
       [], []
  in
  let set_json = `O [ "name", name ; "kind", kind ; "tunes", `A tune_jsons ; "errors", `A error_jsons ] in
  let all_tune_jsons =
    Tune.Database.get_all ()
    |> List.map (fun (_, tune, version) -> (* FIXME: (tune, version) *)
           Tune.tune_version_to_jsonm (tune, version))
  in
  Lwt.return (`O [ "set", set_json ; "all_tunes", `A all_tune_jsons ])
