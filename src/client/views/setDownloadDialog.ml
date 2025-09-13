open Nes
open Common
open Html
open Model

(* REVIEW: This is close to `VersionDownloadDialog.t`; there is room for
   factorisation here. *)
type t = {
  choice_rows: Html_types.div elt list;
  parameters_signal: (SetParameters.t * RenderingParameters.t) React.signal;
}

let lift_version_parameters every_version =
  SetParameters.make ~every_version ()

let create () =
  let%lwt version_dialog = VersionDownloadDialog.create () in
  let%lwt headers_choices =
    Components.Choices.(
      make_radios
        ~label: "Show headers and footers"
        [
          choice' [txt "Yes"] ~checked: true;
          choice'
            [txt "No"]
            ~value: (
              SetParameters.none,
              RenderingParameters.make ~show_headers: false ()
            );
        ]
    )
  in
  lwt {
    choice_rows = version_dialog.choice_rows @ [
      Components.Component.html headers_choices
    ];
    parameters_signal =
    let no_parameters = (SetParameters.none, RenderingParameters.none) in
    S.merge
      (Pair.map2 SetParameters.compose RenderingParameters.compose)
      no_parameters
      [
        S.map (Pair.map_fst lift_version_parameters) version_dialog.parameters_signal;
        S.map (Option.value ~default: no_parameters % Option.join % Result.to_option) (Components.Component.signal headers_choices);
      ];
  }

(* REVIEW: This is extremely close to `VersionDownloadDialog.render` (apart for
   one line and one type, really); there is room for factorisation here. *)
let open_ set dialog =
  Page.open_dialog @@ fun return ->
  Page.make'
    ~title: (lwt "Download a PDF")
    [div dialog.choice_rows]
    ~buttons: [
      Utils.Button.cancel' ~return ();
      Utils.Button.download
        ~onclick: (fun () ->
          let (set_params, rendering_params) = S.value dialog.parameters_signal in
          return None;
          VersionDownloadDialog.open_pdf_generation_dialog (
            Job.run
              (Entry.Slug.add_suffix (Set.slug' set) ".pdf")
              Endpoints.Api.(route @@ Set BuildPdf)
              (Entry.id set)
              set_params
              rendering_params
          )
        )
        ();
    ]

let create_and_open set = open_ set =<< create ()
