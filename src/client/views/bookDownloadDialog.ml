open Js_of_ocaml
open Nes
open Common
open Model
open Html
open Components

(* REVIEW: This is close to `VersionDownloadDialog.t`; there is room for
   factorisation here. *)
type t = {
  choice_rows: Html_types.div elt list;
  parameters_signal: (BookParameters.t * RenderingParameters.t) React.signal;
}

let lift_set_parameters every_set =
  BookParameters.make ~every_set ()

let create () =
  let%lwt set_dialog = SetDownloadDialog.create () in
  let%lwt booklet_choices =
    Choices.(
      make_radios
        ~label: "Mode"
        [
          choice' [txt "Normal"] ~checked: true;
          choice' [txt "Simple"] ~value: (BookParameters.make ~simple: true ());
        ]
    )
  in
  lwt {
    choice_rows = (
      set_dialog.choice_rows @ [
        Component.html booklet_choices;
      ]
    );
    parameters_signal =
    S.merge
      (Pair.map2_both BookParameters.compose RenderingParameters.compose)
      (BookParameters.none, RenderingParameters.none)
      [
        S.map (Pair.map_fst lift_set_parameters) set_dialog.parameters_signal;
        S.map (Pair.snoc RenderingParameters.none % Option.value ~default: BookParameters.none % Option.join % Result.to_option) (Component.signal booklet_choices);
      ]
  }

(* REVIEW: This is extremely close to `VersionDownloadDialog.render` (apart for
   one line and one type, really); there is room for factorisation here. *)
let open_ book dialog =
  Page.open_dialog @@ fun return ->
  Page.make'
    ~title: (lwt "Download a PDF")
    [div dialog.choice_rows]
    ~buttons: [
      Utils.Button.cancel' ~return ();
      Utils.Button.download
        ~onclick: (fun () ->
          let (book_params, rendering_params) = S.value dialog.parameters_signal in
          let%lwt href = Job.file_href (Book.slug' book) Endpoints.Api.(route @@ Book BuildPdf) (Entry.id book) book_params rendering_params in
          Dom_html.window##.location##.href := Js.string href;
          lwt_unit
        )
        ();
    ]

let create_and_open book = open_ book =<< create ()
