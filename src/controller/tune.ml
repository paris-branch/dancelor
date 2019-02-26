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

let match_score needle haystack =
  let needle = Slug.from_string needle in
  let haystack = Slug.from_string haystack in
  1. -.
  if String.length needle = 0 then
    0.
  else
    let d = String.inclusion_distance needle haystack in
    (float_of_int d) /. (float_of_int (String.length needle))

let get_all query =
  Log.debug (fun m -> m "controller get_all");
  let tune_jsons =
    let name = query_string_opt query "name" in
    let author = query_string_opt query "author" in
    let kind =
      let kind = query_string_or query "kind" "" in
      if kind = "" then
        None
      else
        try
          Some (Kind.base_of_string kind)
        with
          Failure _ -> error "kind must be 'j', 'p', 'r', 's' or 'w'"
    in
    let keys =
      let open Option in
      query_string_opt query "keys" >>= fun keys ->
      let keys = String.split_on_char ',' keys in
      Some (List.map Music.key_of_string keys)
    in
    let mode =
      let open Option in
      query_string_opt query "mode" >>= function
      | "major" -> Some Music.Major
      | "minor" -> Some Music.Minor
      | other -> error ("mode must be major or minor, got " ^ other)
    in
    let hard_limit = query_int_or query "hard-limit" max_int in
    let threshold = (query_float_or query "threshold" 0.) /. 100. in

    Tune.Database.get_all ()
    |> List.map (fun tune -> (1., tune))
    |> List.map
      (match name with
       | None -> fun x -> x
       | Some name ->
         (fun (score, tune) ->
            (score *. match_score name (TuneGroup.name (Tune.group tune)), tune)))
    |> List.map
      (match author with
       | None -> fun x -> x
       | Some author ->
         (fun (score, tune) ->
            match TuneGroup.author (Tune.group tune) with
            | None -> (0., tune)
            | Some author' ->
              (score *. match_score author (Credit.line author'), tune)))
    |> List.filter
      (match kind with
       | None -> fun _ -> true
       | Some kind -> (fun (_, tune) -> TuneGroup.kind (Tune.group tune) = kind))
    |> List.filter
      (match keys with
       | None -> fun _ -> true
       | Some keys -> (fun (_, tune) -> List.mem (Tune.key tune) keys))
    |> List.filter
      (match mode with
       | None -> fun _ -> true
       | Some mode -> (fun (_, tune) -> snd (Tune.key tune) = mode))
    |> List.filter
      (fun (score, _) -> score >= threshold)
    |> List.sort
      (fun (score1, tune1) (score2, tune2) ->
         let c = - compare score1 score2 in (* Scores in decreasing order *)
         if c = 0 then
           compare (Tune.slug tune1) (Tune.slug tune2)
         else
           c)
    |> List.sub hard_limit
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
