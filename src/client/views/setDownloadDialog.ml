open Nes
open Common

open Html
open Model
open Components

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
  Page.open_dialog @@ fun return ->
  Page.make'
    ~title: (Lwt.return "Download a PDF")
    [table dialog.choice_rows]
    ~buttons: [
      Button.cancel' ~return ();
      Button.download ~href: (S.map (fun params -> Endpoints.Api.(href @@ Set Pdf) slug params RenderingParameters.none) dialog.parameters_signal) ();
    ]

let create_and_open slug = open_ slug (create ())
