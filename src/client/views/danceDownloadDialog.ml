open Common

open Html
open Components

let create = SetDownloadDialog.create

let open_ slug dialog =
  Page.open_dialog' @@ fun return ->
  Page.make
    ~title: (S.const "Download a PDF")
    [table dialog.SetDownloadDialog.choice_rows]
    ~buttons: [
      Button.cancel' ~return ();
      Button.download ~href: (S.map (fun params -> Endpoints.Api.(href @@ Dance Pdf) params slug) dialog.SetDownloadDialog.parameters_signal) ();
    ]

let create_and_open slug = open_ slug (create ())
