open Nes
open Dancelor_server_model
open QueryHelpers
module Log = (val Dancelor_server_logs.create "controller.set" : Logs.LOG)

let get set _ =
  Set.get set

let delete set : unit Controller.t = fun _ ->
  let%lwt set = Set.get set in
  Set.delete set

let save : Set.t Controller.t = fun query ->
  Log.debug (fun m -> m "Controller save");
  let%lwt slug =
    try%lwt
      let%lwt slug = query_string query "slug" in
      Lwt.return_some slug
    with
      Dancelor_common.Error.(Exn (BadQuery _)) ->
      Lwt.return_none
  in
  let%lwt name = query_string query "name" in
  let%lwt kind = query_string query "kind" in
  let kind = Kind.dance_of_string kind in
  let%lwt status =
    try%lwt
      let%lwt status = query_string query "status" in
      Lwt.return_some (Status.from_string status)
    with
      Dancelor_common.Error.(Exn (BadQuery _)) ->
      Lwt.return_none
  in
  let%lwt tunes = query_strings query "tunes" in
  let%lwt tunes = Lwt_list.map_s Tune.get tunes in
  Set.save ?slug ~name ~kind ?status ~tunes ()

let get_all : Set.t list Controller.t = fun query ->
  let%lwt contains_tune =
    match%lwt query_string_opt query "contains" with
    | None -> Lwt.return (fun _ -> true)
    | Some tune -> Lwt.return (Set.contains tune)
  in
  let%lwt all = Set.get_all () in
  all
  |> List.filter contains_tune
  |> List.sort (fun s1 s2 -> compare (Set.slug s1) (Set.slug s2))
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
    let%lwt set = Set.get set in
    let%lwt transpose_target = query_string_opt query "transpose-target" in
    let lilypond = render ?transpose_target set in
    Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:lilypond ()
end

module Pdf = struct
  let cache : (Set.t, string Lwt.t) Cache.t = Cache.create ()

  let render ?transpose_target set =
    Cache.use
      cache set
      (fun () ->
        let lilypond = Ly.render ?transpose_target set in
        let path = Filename.concat !Dancelor_server_config.cache "set" in
        let%lwt (fname_ly, fname_pdf) =
          let%lwt slug = Set.slug set in
          let fname = spf "%s-%x" slug (Random.int (1 lsl 29)) in
          Lwt.return (fname^".ly", fname^".pdf")
        in
        let%lwt () =
          Lwt_io.with_file ~mode:Output (Filename.concat path fname_ly)
            (fun ochan -> Lwt_io.write ochan lilypond)
        in
        Log.debug (fun m -> m "Processing with Lilypond");
        let%lwt () = Lilypond.run ~exec_path:path fname_ly in
        let path_pdf = Filename.concat path fname_pdf in
        Lwt.return path_pdf)

  let get set query =
    let%lwt set = Set.get set in
    let%lwt transpose_target = query_string_opt query "transpose-target" in
    let%lwt path_pdf = render ?transpose_target set in
    Cohttp_lwt_unix.Server.respond_file ~fname:path_pdf ()
end
