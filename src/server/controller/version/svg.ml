open NesUnix
open Common

module Log = (val Logger.create "controller.version.svg": Logs.LOG)

let cache : ([`Svg] * Model.Version.t Entry.t * Model.VersionParameters.t * string * RenderingParameters.t, string Lwt.t) StorageCache.t =
  StorageCache.create ()

let populate_cache () =
  ControllerCache.populate ~cache ~type_: "version" ~ext: ".svg" ~pp_ext: "svg"

let render version version_parameters rendering_parameters =
  let body = Model.Version.content' version in
  StorageCache.use ~cache ~key: (`Svg, version, version_parameters, body, rendering_parameters) @@ fun hash ->
  Log.debug (fun m -> m "Rendering the LilyPond version");
  let%lwt (fname_ly, fname_svg) =
    let slug = Entry.slug version in
    let fname = aspf "%a-%a" Slug.pp' slug StorageCache.pp_hash hash in
    lwt (fname ^ ".ly", fname ^ ".svg")
  in
  Log.debug (fun m -> m "LilyPond file name: %s" fname_ly);
  Log.debug (fun m -> m "SVG file name: %s" fname_svg);
  let path = Filename.concat !Config.cache "version" in
  Log.debug (fun m -> m "Preparing lilypond file");
  Ly.prepare_file version_parameters ~show_meta: false ~fname: (Filename.concat path fname_ly) version;%lwt
  Log.debug (fun m -> m "Generate score");
  LilyPond.svg
    ~exec_path: path
    ~fontconfig_file: (Filename.concat !Config.share "fonts.conf")
    ~stylesheet: "/fonts.css"
    fname_ly;%lwt
  Log.debug (fun m -> m "done!");
  lwt (Filename.concat path fname_svg)

let get env version version_parameters rendering_parameters =
  Log.debug (fun m -> m "Model.Version.Svg.get %a" Slug.pp' version);
  let%lwt version = Model.Version.get version in
  Permission.assert_can_get env version;%lwt
  let%lwt path_svg = render version version_parameters rendering_parameters in
  Madge_server.respond_file ~content_type: "image/svg+xml" ~fname: path_svg

let preview_slug = Slug.check_string_exn "preview"
let preview env version version_parameters rendering_parameters =
  Log.debug (fun m -> m "Model.Version.Svg.preview");
  Permission.assert_can_create env;%lwt
  let version = Entry.make ~slug: preview_slug version in
  let%lwt path_svg = render version version_parameters rendering_parameters in
  Madge_server.respond_file ~content_type: "image/svg+xml" ~fname: path_svg
