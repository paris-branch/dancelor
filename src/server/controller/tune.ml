open Nes
open Dancelor_server_model
open QueryHelpers
module Log = (val Dancelor_server_logs.create "controller.tune" : Logs.LOG)

let get tune _ =
  Tune.get tune

let get_ly tune _ =
  let%lwt tune = Tune.get tune in
  let%lwt body = Tune.content tune in
  Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body ()

let get_all : Tune.t Score.t list Controller.t = fun query ->
  Log.debug (fun m -> m "controller get_all");
  let%lwt name = query_string_opt query "name" in
  let%lwt author = query_string_opt query "author" in
  let%lwt kind =
    match%lwt query_string_opt query "kind" with
    | None -> Lwt.return_none
    | Some kind ->
      try
        Lwt.return_some (Kind.base_of_string kind)
      with
        Failure _ ->
        Lwt.fail Dancelor_common.Error.(Exn (BadQuery "kind must be 'j', 'p', 'r', 's' or 'w'"))
  in
  let%lwt keys =
    match%lwt query_string_opt query "keys" with
    | None -> Lwt.return_none
    | Some keys ->
      let keys = String.split_on_char ',' keys in
      Lwt.return_some (List.map Music.key_of_string keys)
  in
  let%lwt mode =
    match%lwt query_string_opt query "mode" with
    | None -> Lwt.return_none
    | Some "major" -> Lwt.return_some Music.Major
    | Some "minor" -> Lwt.return_some Music.Minor
    | Some other -> Lwt.fail Dancelor_common.Error.(Exn (BadQuery ("mode must be 'major' or 'minor', got " ^ other)))
  in
  let%lwt hard_limit = query_int_opt query "hard-limit"  in
  let%lwt threshold = query_float_opt query "threshold" in
  Tune.get_all ?kind ?keys ?mode ?name ?author ?hard_limit ?threshold ()

module Png = struct
  let cache : (Tune.t, string Lwt.t) Cache.t = Cache.create ()

  (* let template =
    let path = Filename.concat_l [!Dancelor_server_config.share; "lilypond"; "tune.ly"] in
    Log.debug (fun m -> m "Loading template file %s" path);
    let ichan =  open_in path in
    let template = Lexing.from_channel ichan |> Mustache.parse_lx in
    close_in ichan;
    Log.debug (fun m -> m "Loaded successfully");
    template *)

  let render tune =
    Cache.use
      cache tune
      (fun () ->
         Log.debug (fun m -> m "Rendering the Lilypond version");
         let _json = Tune.to_yojson tune in
         assert false (* FIXME *)
         (* let lilypond = Mustache.render template (Json.to_ezjsonm json) in
         let path = Filename.concat !Dancelor_server_config.cache "tune" in
         let fname_ly, fname_png =
           let fname = spf "%s-%x" (Tune.slug tune) (Random.int (1 lsl 29)) in
           (fname^".ly", fname^".png")
         in
         Lwt_io.with_file ~mode:Output (Filename.concat path fname_ly)
           (fun ochan -> Lwt_io.write ochan lilypond) >>= fun () ->
         Log.debug (fun m -> m "Processing with Lilypond");
         Lilypond.run ~exec_path:path ~options:["-dresolution=110"; "-dbackend=eps"; "--png"] fname_ly
         >>= fun () ->
            Lwt.return (Filename.concat path fname_png)) *))

  let get tune _ =
    let%lwt tune = Tune.get tune in
    let%lwt path_png = render tune in
    Cohttp_lwt_unix.Server.respond_file ~fname:path_png ()
end
