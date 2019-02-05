open Dancelor_common
open Dancelor_model
open QueryHelpers
module Log = (val Log.create "dancelor.controller.tune" : Logs.LOG)

let get tune _ =
  tune
  |> Tune.to_jsonm
  |> (fun json -> Lwt.return (`O ["tune", json]))

let get_ly tune _ =
  Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:(Tune.content tune) ()

let get_all query =
  Log.debug (fun m -> m "controller get_all");
  let tune_jsons =
    Tune.Database.get_all
      ?name:(query_string_opt query "name")
      ?author:(query_string_opt query "author")
      ?kind:(
        let kind = query_string_or query "kind" "" in
        if kind = "" then
          None
        else
          try
            Some (Kind.base_of_string kind)
          with
            Failure _ -> error "kind must be 'j', 'p', 'r', 's' or 'w'"
      )
      ?keys:(
        let open Option in
        query_string_opt query "keys" >>= fun keys ->
        let keys = String.split_on_char ',' keys in
        Some (List.map Music.key_of_string keys)
      )
      ?mode:(
        let open Option in
        query_string_opt query "mode" >>= function
        | "major" -> Some Music.Major
        | "minor" -> Some Music.Minor
        | other -> error ("mode must be major or minor, got " ^ other)
      )
      ?hard_limit:(query_int_opt query "hard-limit")
      ?threshold:(query_float_opt query "threshold")
      ()
    |> List.map (fun (score, tune) ->
           Tune.to_json tune
           |> Json.add_field "score" (`Float (floor (100. *. score)))
           |> Json.to_value)
    |> (fun jsons -> `A jsons)
  in
  Lwt.return (
      `O [
          "tunes", tune_jsons;
          "query",
          `O [
              "name", `String (query_string_or query "name" "");
              "author", `String (query_string_or query "author" "");
            ]
        ]
    )

module Png = struct
  let cache : (Tune.t, string Lwt.t) Cache.t = Cache.create ()

  let template =
    let path = Filename.concat_l [!Config.share; "lilypond"; "tune.ly"] in
    Log.debug (fun m -> m "Loading template file %s" path);
    let ichan =  open_in path in
    let template = Lexing.from_channel ichan |> Mustache.parse_lx in
    close_in ichan;
    Log.debug (fun m -> m "Loaded successfully");
    template

  let (>>=) = Lwt.bind

  let render tune =
    Cache.use
      cache tune
      (fun () ->
        Log.debug (fun m -> m "Rendering the Lilypond version");
        let json = Tune.to_json tune in
        let lilypond = Mustache.render template (Json.to_ezjsonm json) in
        let path = Filename.concat !Config.cache "tune" in
        let fname_ly, fname_png =
          let fname = spf "%s-%x" (Tune.slug tune) (Random.int (1 lsl 29)) in
          (fname^".ly", fname^".png")
        in
        Lwt_io.with_file ~mode:Output (Filename.concat path fname_ly)
          (fun ochan -> Lwt_io.write ochan lilypond) >>= fun () ->
        Log.debug (fun m -> m "Processing with Lilypond");
        Lilypond.run ~exec_path:path ~options:["-dresolution=110"; "-dbackend=eps"; "--png"] fname_ly
        >>= fun () ->
        Lwt.return (Filename.concat path fname_png))

  let get tune _ =
    render tune >>= fun path_png ->
    Cohttp_lwt_unix.Server.respond_file ~fname:path_png ()
end
