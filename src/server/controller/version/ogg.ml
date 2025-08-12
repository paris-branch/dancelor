open NesUnix
open Common

module Log = (val Logger.create "controller.version.ogg": Logs.LOG)

let cache : ([`Ogg] * Model.Version.t Entry.t * Model.VersionParameters.t * string * RenderingParameters.t, string Lwt.t) StorageCache.t =
  StorageCache.create ()

let populate_cache () =
  ControllerCache.populate ~cache ~type_: "version" ~ext: ".ogg" ~pp_ext: "ogg"

let render version version_parameters rendering_parameters =
  let body = Model.Version.content' version in
  StorageCache.use ~cache ~key: (`Ogg, version, version_parameters, body, rendering_parameters) @@ fun hash ->
  let%lwt (fname_ly, fname_ogg) =
    let id = Entry.id version in
    let fname = aspf "%a-%a" Entry.Id.pp' id StorageCache.pp_hash hash in
    lwt (fname ^ ".ly", fname ^ ".ogg")
  in
  let path = Filename.concat !Config.cache "version" in
  Ly.prepare_file ~fname: (Filename.concat path fname_ly) version_parameters version;%lwt
  Log.debug (fun m -> m "Processing with LilyPond");
  LilyPond.ogg
    ~exec_path: path
    ~fontconfig_file: (Filename.concat !Config.share "fonts.conf")
    fname_ly;%lwt
  lwt (Filename.concat path fname_ogg)

let get env id _slug version_parameters rendering_parameters =
  Log.debug (fun m -> m "Model.Version.Ogg.get %a" Entry.Id.pp' id);
  match%lwt Database.Version.get id with
  | None -> Permission.reject_can_get ()
  | Some version ->
    Permission.assert_can_get env version;%lwt
    let%lwt path_ogg = render version version_parameters rendering_parameters in
    Madge_server.respond_file ~content_type: "audio/ogg" ~fname: path_ogg

let preview env version version_parameters rendering_parameters =
  Log.debug (fun m -> m "Model.Version.Ogg.preview");
  Permission.assert_can_create env;%lwt
  let version = Entry.make ~id: (Entry.Id.make ()) version in
  let%lwt path_ogg = render version version_parameters rendering_parameters in
  Madge_server.respond_file ~content_type: "audio/ogg" ~fname: path_ogg
