open NesUnix
open Common

module Log = (val Logger.create "controller.version.ogg": Logs.LOG)

let cache : ([`Ogg] * Model.Version.t Entry.t * Model.VersionParameters.t * string, string Lwt.t) StorageCache.t =
  StorageCache.create ()

let populate_cache () =
  ControllerCache.populate ~cache ~type_: "version" ~ext: ".ogg" ~pp_ext: "ogg"

let render parameters version =
  let body = Model.Version.content version in
  StorageCache.use ~cache ~key: (`Ogg, version, parameters, body) @@ fun hash ->
  let%lwt (fname_ly, fname_ogg) =
    let slug = Entry.slug version in
    let fname = aspf "%a-%a" Slug.pp' slug StorageCache.pp_hash hash in
    Lwt.return (fname ^ ".ly", fname ^ ".ogg")
  in
  let path = Filename.concat !Config.cache "version" in
  Ly.prepare_file ~fname: (Filename.concat path fname_ly) parameters version;%lwt
  Log.debug (fun m -> m "Processing with LilyPond");
  LilyPond.ogg
    ~exec_path: path
    ~fontconfig_file: (Filename.concat !Config.share "fonts.conf")
    fname_ly;%lwt
  Lwt.return (Filename.concat path fname_ogg)

let get env parameters version =
  Log.debug (fun m -> m "Model.Version.Ogg.get %a" Slug.pp' version);
  let%lwt version = Model.Version.get version in
  Permission.assert_can_get env version;%lwt
  let%lwt path_ogg = render parameters version in
  Madge_server.shortcut @@ Cohttp_lwt_unix.Server.respond_file ~fname: path_ogg ()

let preview_slug = Slug.check_string_exn "preview"
let preview env parameters version =
  Log.debug (fun m -> m "Model.Version.Ogg.preview");
  Permission.assert_can_create env;%lwt
  let version = Entry.make ~slug: preview_slug version in
  let%lwt path_ogg = render parameters version in
  Madge_server.shortcut @@ Cohttp_lwt_unix.Server.respond_file ~fname: path_ogg ()
