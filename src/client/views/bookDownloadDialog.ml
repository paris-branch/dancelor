open Nes
open Common
open Model
open Html
open Components

(* REVIEW: This is close to `VersionDownloadDialog.t`; there is room for
   factorisation here. *)
type t = {
  choice_rows: Html_types.div elt list;
  parameters_signal: (Book_parameters.t * Rendering_parameters.t) React.signal;
}

let lift_set_parameters every_set =
  Book_parameters.make ~every_set ()

let create () =
  let%lwt set_dialog = SetDownloadDialog.create () in
  let%lwt booklet_choices =
    Choices.(
      make_radios
        ~label: "Mode"
        [
          choice' [txt "Normal"] ~checked: true;
          choice' [txt "Simple"] ~value: (Book_parameters.make ~simple: true ());
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
      (Pair.map2 Book_parameters.compose Rendering_parameters.compose)
      (Book_parameters.none, Rendering_parameters.none)
      [
        S.map (Pair.map_fst lift_set_parameters) set_dialog.parameters_signal;
        S.map (Pair.snoc Rendering_parameters.none % Option.value ~default: Book_parameters.none % Option.join % Result.to_option) (Component.signal booklet_choices);
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
          return None;
          VersionDownloadDialog.open_pdf_generation_dialog (
            Job.run
              (NesSlug.add_suffix (Book.slug' book) ".pdf")
              Endpoints.Api.(route @@ Book BuildPdf)
              (Entry.id book)
              book_params
              rendering_params
          )
        )
        ();
    ]

let create_and_open book = open_ book =<< create ()
