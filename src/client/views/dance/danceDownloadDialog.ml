open Dancelor_common
open Dancelor_client_html
open Dancelor_client_components

let create = SetDownloadDialog.create

let open_ slug dialog =
  Dialog.open_ @@ fun return -> [
    h2 ~a:[a_class ["title"]] [txt "Download a PDF"];

    form [
      table dialog.SetDownloadDialog.choice_rows;

      a
        ~a:[
          a_class ["button"];
          a_target "_blank";
          R.a_href (S.map ApiRouter.(fun params -> path_dancePdf ?params slug) dialog.parameters_signal);
          a_onclick (fun _ -> return (); true);
        ] [txt "Download"];
    ];
  ]

let create_and_open slug = open_ slug (create ())
