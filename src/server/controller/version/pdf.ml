open NesUnix
open Common

module Log = (val Logger.create "controller.version.pdf": Logs.LOG)

let render version version_parameters rendering_parameters =
  let%lwt kind = Model.Version.kind version in
  let%lwt name = Model.Version.name version in
  let%lwt set_parameters =
    Model.SetParameters.make
      ~show_order: false
      ?display_name: (Model.VersionParameters.display_name version_parameters)
      ()
  in
  let parameters = Model.VersionParameters.set_display_name "" version_parameters in
  let set =
    Entry.make_dummy @@
      Model.Set.make
        ~name
        ~kind: (Kind.Dance.Version kind)
        ~contents: [(version, parameters)]
        ~order: [Internal 1]
        ()
  in
  Set.Pdf.render set set_parameters rendering_parameters

let get env version version_parameters rendering_parameters =
  let%lwt version = Model.Version.get version in
  Permission.assert_can_get env version;%lwt
  let%lwt path_pdf = render version version_parameters rendering_parameters in
  Madge_server.respond_file ~content_type: "application/pdf" ~fname: path_pdf
