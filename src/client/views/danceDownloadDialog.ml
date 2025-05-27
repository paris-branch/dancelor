open Nes
open Common

open Html
open Components

let create = SetDownloadDialog.create

let open_ slug dialog =
  Page.open_dialog' @@ fun return ->
  Page.make'
    ~title: (lwt "Download a PDF")
    [table dialog.SetDownloadDialog.choice_rows]
    ~buttons: [
      Button.cancel' ~return ();
      Button.download ~href: (S.map (fun params -> Endpoints.Api.(href @@ Dance Pdf) slug params RenderingParameters.none) dialog.SetDownloadDialog.parameters_signal) ();
    ]

let create_and_open slug = open_ slug (create ())
