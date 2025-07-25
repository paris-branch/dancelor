open Nes
open Common

open Html
open Model
open Components

let create = SetDownloadDialog.create

let open_ dance dialog =
  Page.open_dialog' @@ fun return ->
  Page.make'
    ~title: (lwt "Download a PDF")
    [table dialog.SetDownloadDialog.choice_rows]
    ~buttons: [
      Button.cancel' ~return ();
      Button.download ~href: (S.map (fun params -> Endpoints.Api.(href @@ Dance Pdf) (Entry.id dance) (Dance.slug' dance) params RenderingParameters.none) dialog.SetDownloadDialog.parameters_signal) ();
    ]

let create_and_open dance = open_ dance (create ())
