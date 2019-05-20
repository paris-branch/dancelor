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

  let render ?transpose_target set =
    let (transpose, target, instrument) =
      (* transpose introduces a comment when we don't want to transpose *)
      match transpose_target with
      | None -> ("%", "c", "C")
      | Some target ->
        let instrument =
          match target with
          | "bes," -> "B flat"
          | "ees" -> "E flat"
          | _ -> target
        in
        ("", target, instrument)
    in
    let (res, prom) =
      Format.with_formatter_to_string_gen @@ fun fmt ->
      let%lwt title = Set.name set in
      let%lwt kind = Set.kind set in
      let%lwt tunes = Set.tunes set in
      fpf fmt [%blob "template/version.ly"];
      fpf fmt [%blob "template/set/header.ly"]
        title (Kind.dance_to_string kind)
        transpose instrument;
      fpf fmt [%blob "template/layout.ly"];
      fpf fmt [%blob "template/set/paper.ly"];
      Lwt_list.iter_s
        (fun tune ->
           let%lwt content = Tune.content tune in
           let%lwt group = Tune.group tune in
           let%lwt name = TuneGroup.name group in
           let%lwt author =
             match%lwt TuneGroup.author group with
             | None -> Lwt.return ""
             | Some author -> Credit.line author
           in
           fpf fmt [%blob "template/set/tune.ly"]
             name author
             transpose target
             content
             transpose;
           Lwt.return ())
        tunes
    in
    let%lwt () = prom in
    Lwt.return res

  let get set query =
    let%lwt set = Set.get set in
    let%lwt transpose_target = query_string_opt query "transpose-target" in
    let%lwt lilypond = render ?transpose_target set in
    Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:lilypond ()
end

module Pdf = struct
  let cache : (Set.t, string Lwt.t) Cache.t = Cache.create ()

  let render ?transpose_target set =
    Cache.use
      cache set
      (fun () ->
        let%lwt lilypond = Ly.render ?transpose_target set in
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
