open Common

open Html
open Components

let create = SetDownloadDialog.create

let open_ slug dialog =
  Dialog.open_ @@ fun return ->
  [
    h2 ~a: [a_class ["title"]] [txt "Download a PDF"];
    form
      [
        table dialog.SetDownloadDialog.choice_rows;
        a
          ~a: [
            a_class ["button"];
            a_target "_blank";
            R.a_href (S.map (fun params -> Endpoints.Api.(href @@ Dance Pdf) params slug) dialog.parameters_signal);
            a_onclick (fun _ -> return (); true);
          ]
          [txt "Download"];
      ];
  ]

let create_and_open slug = open_ slug (create ())
