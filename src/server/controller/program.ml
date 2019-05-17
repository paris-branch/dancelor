open Nes
open Dancelor_server_model
open QueryHelpers
module Log = (val Dancelor_server_logs.create "controller.program" : Logs.LOG)

let get program : Program.t Controller.t = fun _ ->
  Dancelor_database.Program.get program

let get_all : Program.t list Controller.t = fun query ->
  let%lwt contains_set =
    try%lwt
      let%lwt set = query_string query "contains" in
      Lwt.return (Program.contains set)
    with Dancelor_common.Error.(Exn (BadQuery _)) ->
      Lwt.return (fun _ -> true)
  in
  let%lwt all = Dancelor_database.Program.get_all () in
  all
  |> List.filter contains_set
  |> Lwt.return

module Ly = struct
  (* let template =
    let path = Filename.concat_l [!Dancelor_server_config.share; "lilypond"; "program.ly"] in
    Log.debug (fun m -> m "Loading template file %s" path);
    let ichan = open_in path in
    let template = Lexing.from_channel ichan |> Mustache.parse_lx in
    close_in ichan;
    Log.debug (fun m -> m "Loaded successfully");
    template *)

  let render ?transpose_target program =
    Program.to_yojson program
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
    |> (fun _ -> assert false)
end

module Pdf = struct
  let cache : (Program.t, string Lwt.t) Cache.t = Cache.create ()

  let (>>=) = Lwt.bind

  let render ?transpose_target program =
    Cache.use
      cache program
      (fun () ->
        let lilypond = Ly.render ?transpose_target program in
        let path = Filename.concat !Dancelor_server_config.cache "program" in
        let%lwt (fname_ly, fname_pdf) =
          let%lwt slug = Program.slug program in
          let fname = spf "%s-%x" slug (Random.int (1 lsl 29)) in
          Lwt.return (fname^".ly", fname^".pdf")
        in
        Lwt_io.with_file ~mode:Output (Filename.concat path fname_ly)
          (fun ochan -> Lwt_io.write ochan lilypond) >>= fun () ->
        Log.debug (fun m -> m "Processing with Lilypond");
        Lilypond.run ~exec_path:path fname_ly >>= fun () ->
        let path_pdf = Filename.concat path fname_pdf in
        Lwt.return path_pdf)

  let get program query =
    let%lwt program = Dancelor_database.Program.get program in
    let%lwt transpose_target =
      try%lwt
        let%lwt transpose_target = query_string query "transpose-target" in
        Lwt.return_some transpose_target
      with
        Dancelor_common.Error.(Exn (BadQuery _)) ->
        Lwt.return_none
    in
    let%lwt path_pdf = render ?transpose_target program in
    Cohttp_lwt_unix.Server.respond_file ~fname:path_pdf ()
end
