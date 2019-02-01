open Dancelor_common
open Dancelor_model
open QueryHelpers
module Log = (val Log.create "dancelor.controller.set" : Logs.LOG)

let get uri _ =
  Log.debug (fun m -> m "controller get");
  let slug = List.assoc "slug" uri in
  try
    Set.Database.get slug
    |> Set.to_jsonm
    |> (fun json -> Lwt.return (`O ["set", json]))
  with
    Not_found ->
    error ("this set does not exist: " ^ slug)

let get_all _ _ =
  Log.debug (fun m -> m "controller get_all");
  Set.Database.get_all ()
  |> List.sort (fun s1 s2 -> compare (Set.slug s1) (Set.slug s2))
  |> List.map Set.to_jsonm
  |> (fun json -> Lwt.return (`O ["sets", `A json]))

module Ly = struct
  let template =
    let path = Filename.concat_l [!Config.share; "lilypond"; "set.ly"] in
    Log.debug (fun m -> m "Loading template file %s" path);
    let ichan = open_in path in
    let template = Lexing.from_channel ichan |> Mustache.parse_lx in
    close_in ichan;
    Log.debug (fun m -> m "Loaded successfully");
    template

  let render ?transpose_target set =
    Set.to_json set
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

  let get uri query =
    Log.debug (fun m -> m "controller Ly.get");
    let slug = List.assoc "slug" uri in
    try
      let set = Set.Database.get slug in
      let lilypond = render ?transpose_target:(query_string_opt query "transpose-target") set in
      Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:lilypond ()
    with
      Not_found ->
      error ("this set does not exist: " ^ slug)
end

module Pdf = struct
  let cache : (Set.t, string Lwt.t) Cache.t = Cache.create ()

  let (>>=) = Lwt.bind

  let render ?transpose_target set =
    Cache.use
      cache set
      (fun () ->
        let lilypond = Ly.render ?transpose_target set in
        let path = Filename.concat !Config.cache "set" in
        let fname_ly, fname_pdf =
          let fname = spf "%s-%x" (Set.slug set) (Random.int (1 lsl 29)) in
          (fname^".ly", fname^".pdf")
        in
        Lwt_io.with_file ~mode:Output (Filename.concat path fname_ly)
          (fun ochan -> Lwt_io.write ochan lilypond) >>= fun () ->
        Log.debug (fun m -> m "Processing with Lilypond");
        Lilypond.run ~exec_path:path fname_ly >>= fun () ->
        let path_pdf = Filename.concat path fname_pdf in
        Lwt.return path_pdf)

  let get uri query =
    Log.debug (fun m -> m "controller Pdf.get");
    let slug = List.assoc "slug" uri in
    try
      let set = Set.Database.get slug in
      render ?transpose_target:(query_string_opt query "transpose-target") set >>= fun path_pdf ->
      Cohttp_lwt_unix.Server.respond_file ~fname:path_pdf ()
    with
      Not_found ->
      error ("this set does not exist: " ^ slug)
end

let save _ query =
  Log.debug (fun m -> m "controller save");
  let name = query_string query "name" in
  let kind = Kind.dance_of_string (query_string query "kind") in
  let tunes =
    query_strings query "tunes"
    |> List.map Tune.Database.get
  in
  Set.Database.create ~name ~kind ~tunes ()
  |> Set.to_jsonm
  |> (fun json -> Lwt.return (`O ["set", json]))
