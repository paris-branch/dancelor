open Common

open Html

let create = SetDownloadDialog.create

let open_ slug dialog =
  Page.open_dialog' @@ fun return ->
  Page.make
    ~title: (S.const "Download a PDF")
    [table dialog.SetDownloadDialog.choice_rows]
    ~buttons: [
      a
        ~a: [
          a_class ["button"];
          a_target "_blank";
          R.a_href (S.map (fun params -> Endpoints.Api.(href @@ Dance Pdf) params slug) dialog.SetDownloadDialog.parameters_signal);
          a_onclick (fun _ -> return (); true);
        ]
        [txt "Download"];
    ]

let create_and_open slug = open_ slug (create ())
