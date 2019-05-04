open Nes open Option
open Dancelor_server_model
open QueryHelpers
module Log = (val Dancelor_server_logs.create "controller.set" : Logs.LOG)

let get set _ =
  set
  |> Dancelor_database.Set.get
  |> Set.to_yojson
  |> Lwt.return

let delete set _ =
  Dancelor_database.Set.delete set;
  Lwt.return (`List [])

let save query =
  Log.debug (fun m -> m "Controller save");
  let slug = query_string_opt query "slug" in
  let name = query_string query "name" in
  let kind = Dancelor_common_model.Kind.dance_of_string (query_string query "kind") in
  let status = query_string_opt query "status" >>= fun status -> Some (Dancelor_common_model.Status.from_string status) in
  let tunes = query_strings query "tunes" in
  Dancelor_database.Set.save ?slug ~name ~kind ?status ~tunes ()
  |> Set.to_yojson
  |> Lwt.return

let get_all query =
  let contains_tune =
    match query_string_opt query "contains" with
    | None -> (fun _ -> true)
    | Some tune -> Set.contains tune
  in
  Dancelor_database.Set.get_all ()
  |> Seq.filter contains_tune
  |> List.of_seq
  |> List.sort (fun s1 s2 -> compare (Set.slug s1) (Set.slug s2))
  |> List.map Set.to_yojson
  |> (fun json -> `List json)
  |> Lwt.return

module Ly = struct
  (* let template =
    let path = Filename.concat_l [!Dancelor_server_config.share; "lilypond"; "set.ly"] in
    Log.debug (fun m -> m "Loading template file %s" path);
    let ichan = open_in path in
    let template = Lexing.from_channel ichan |> Mustache.parse_lx in
    close_in ichan;
    Log.debug (fun m -> m "Loaded successfully");
    template *)

  let render ?transpose_target set =
    Set.to_yojson set
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
             `Assoc [ "target", `String target ;
                      "instrument", `String instrument ])
    |> (fun _ -> assert false) (* FIXME *)

  let get set query =
    let set = Dancelor_database.Set.get set in
    let lilypond = render ?transpose_target:(query_string_opt query "transpose-target") set in
    Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:lilypond ()
end

module Pdf = struct
  let cache : (Set.t, string Lwt.t) Cache.t = Cache.create ()

  let (>>=) = Lwt.bind

  let render ?transpose_target set =
    Cache.use
      cache set
      (fun () ->
        let lilypond = Ly.render ?transpose_target set in
        let path = Filename.concat !Dancelor_server_config.cache "set" in
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

  let get set query =
    let set = Dancelor_database.Set.get set in
    render ?transpose_target:(query_string_opt query "transpose-target") set >>= fun path_pdf ->
    Cohttp_lwt_unix.Server.respond_file ~fname:path_pdf ()
end
