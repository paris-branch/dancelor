open Nes
open Dancelor_common
open Dancelor_client_model
open Dancelor_client_html
open Dancelor_client_components

(* REVIEW: This is close to `VersionDownloadDialog.t`; there is room for
   factorisation here. *)
type t = {
  choice_rows: Html_types.tr elt list;
  parameters_signal: SetParameters.t React.signal;
}

let lift_version_parameters every_version =
  SetParameters.make ~every_version ()

let create () =
  let version_dialog = VersionDownloadDialog.create () in
  {
    choice_rows = version_dialog.choice_rows;
    parameters_signal = S.map (Option.value ~default: SetParameters.none) @@
      S.merge
        (Option.concat SetParameters.compose)
        None
        [
          S.map (Option.some % lift_version_parameters) version_dialog.parameters_signal;
          (* feels a bit silly at this point but will make more sense once we have set-specific options *)
        ];
  }

(* REVIEW: This is extremely close to `VersionDownloadDialog.render` (apart for
   one line and one type, really); there is room for factorisation here. *)
let open_ slug dialog =
  Dialog.open_ @@ fun return ->
  [
    h2 ~a: [a_class ["title"]] [txt "Download a PDF"];
    form
      [
        table dialog.choice_rows;
        a
          ~a: [
            a_class ["button"];
            a_target "_blank";
            R.a_href (S.map (fun params -> ApiRouter.(href @@ Set Pdf) params slug) dialog.parameters_signal);
            a_onclick (fun _ -> return (); true);
          ]
          [txt "Download"];
      ];
  ]

let create_and_open slug = open_ slug (create ())
