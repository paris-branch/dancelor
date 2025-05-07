open NesUnix
open Common

module Log = (val Logger.create "controller.version.pdf": Logs.LOG)

let render version version_parameters rendering_parameters =
  let%lwt set =
    let%lwt name = Model.Version.name' version in
    let%lwt kind = Model.Version.kind' version in
    let kind = Kind.Dance.Version kind in
    let version_parameters = Model.VersionParameters.set_display_name "" version_parameters in
    let contents = [(version, version_parameters)] in
    let order = [Model.SetOrder.Internal 1] in
    Lwt.return @@ Model.Set.make ~name ~kind ~contents ~order ()
  in
  let%lwt set_parameters =
    let show_order = false in
    let display_name = Model.VersionParameters.display_name version_parameters in
    Model.SetParameters.make ~show_order ?display_name ()
  in
  let%lwt rendering_parameters =
    let%lwt pdf_metadata =
      let%lwt tune = Model.Version.tune' version in
      let name = Option.value (Model.VersionParameters.display_name version_parameters) ~default: (Model.Tune.name' tune) in
      let%lwt composers = Lwt.map (List.map Model.Person.name') @@ Model.Tune.composers' tune in
      let subjects = [KindBase.to_pretty_string ~capitalised: true @@ Model.Tune.kind' tune] in
      Lwt.return @@
        RenderingParameters.update_pdf_metadata
          ~title: (String.replace_empty ~by: name)
          ~composers: (List.replace_nil ~by: composers)
          ~subjects: (List.replace_nil ~by: subjects)
    in
    Lwt.return @@ RenderingParameters.update ~pdf_metadata rendering_parameters
  in
  Set.Pdf.render set set_parameters rendering_parameters

let get env version version_parameters rendering_parameters =
  let%lwt version = Model.Version.get version in
  Permission.assert_can_get env version;%lwt
  let%lwt path_pdf = render version version_parameters rendering_parameters in
  Madge_server.respond_file ~content_type: "application/pdf" ~fname: path_pdf
