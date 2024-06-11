open Dancelor_common
open Dancelor_client_html
open Dancelor_client_components

let create_and_open slug =
  Dialog.open_ @@ fun return -> [
    h2 ~a:[a_class ["title"]] [txt "Download a PDF"];

    form [
      a
        ~a:[
          a_class ["button"];
          a_target "_blank";
          a_href ApiRouter.(path_dancePdf slug);
          a_onclick (fun _ -> return (); true);
        ] [txt "Download"];
    ];
  ]
