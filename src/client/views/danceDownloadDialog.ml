open Nes
open Common
open Html
open Model

let create = SetDownloadDialog.create

let open_ dance dialog =
  Page.open_dialog' @@ fun return ->
  Page.make'
    ~title: (lwt "Download a PDF")
    [table dialog.SetDownloadDialog.choice_rows]
    ~buttons: [
      Utils.Button.cancel' ~return ();
      Utils.Button.download ~href: (S.map (uncurry @@ Endpoints.Api.(href @@ Dance Pdf) (Entry.id dance) (Dance.slug' dance)) dialog.SetDownloadDialog.parameters_signal) ();
    ]

let create_and_open dance = open_ dance =<< create ()
