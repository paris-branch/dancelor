open Dancelor_common
open Dancelor_model
open QueryHelpers

module Log = (val Log.create "dancelor.controller.tune" : Logs.LOG)

let get query =
  let slug = query_string query "slug" in
  try
    Tune.Database.get slug
    |> Tune.view
    |> Tune.view_to_jsonm
    |> (fun json -> Lwt.return (`O ["tune", json]))
  with
    Not_found ->
    raise (Error.Error (`OK, "this tune does not exist"))

let get_ly query =
  let slug = query_string query "slug" in
  try
    let tune = Tune.Database.get slug in
    Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:(Tune.content tune) ()
  with
    Not_found ->
    raise (Error.Error (`OK, "this tune does not exist"))

let get_all query =
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
            Failure _ -> raise (Error.Error (`OK, "kind must be 'j', 'p', 'r', 's' or 'w'"))
      )
      ?keys:(
        let open OptionMonad in
        query_strings_opt query "keys" >>= fun keys ->
        Some (List.map Music.key_of_string keys)
      )
      ?mode:(
        let open OptionMonad in
        query_string_opt query "mode" >>= function
        | "major" -> Some Music.Major
        | "minor" -> Some Music.Minor
        | _ -> error ("mode must be major or minor")
      )
      ()
    |> List.map (fun (score, tune) ->
           Tune.(tune |> view |> view_to_jsonm)
           |> JsonHelpers.add_field "score" (`Float (100. *. score)))
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
  let cache : (Tune.t, string Lwt.t) Hashtbl.t = Hashtbl.create 8

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
    let slug = query_string query "slug" in
    Log.debug (fun m -> m "Finding PNG for %s" slug);
    let tune = Tune.Database.get slug in
    let processor =
      try
        let processor = Hashtbl.find cache tune in
        Log.debug (fun m -> m "Found in cache");
        processor
      with
        Not_found ->
        let processor =
          Log.debug (fun m -> m "Not in the cache. Rendering the Lilypond version");
          let view = Tune.(view_to_jsonm (view tune)) in
          let lilypond = Mustache.render template (`O ["tune", view]) in
          let path = Filename.concat Config.cache "tune" in
          let fname_ly, fname_png =
            let fname = spf "%s-%x" (Tune.slug tune) (Random.int (1 lsl 29)) in
            (fname^".ly", fname^".png")
          in
          Lwt_io.with_file ~mode:Output (Filename.concat path fname_ly)
            (fun ochan -> Lwt_io.write ochan lilypond) >>= fun () ->
          Log.debug (fun m -> m "Processing with Lilypond");
          Lwt_process.exec
            ~env:[|"PATH="^(Unix.getenv "PATH");
                   "LANG=en"|]
            (Lwt_process.shell
               ("cd " ^ path ^ " && " ^ Config.lilypond ^ " -dresolution=110 -dbackend=eps --png " ^ fname_ly)) >>= fun status ->
          assert (status = Unix.WEXITED 0);
          Lwt.return (Filename.concat path fname_png)
        in
        Hashtbl.add cache tune processor;
        processor
    in
    processor >>= fun path_png ->
    Cohttp_lwt_unix.Server.respond_file ~fname:path_png ()
end
