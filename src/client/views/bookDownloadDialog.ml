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
          choice'
            [txt "Booklet"]
            ~value: (
              BookParameters.make
                ()
            );
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
      Utils.Button.download ~href: (S.map (uncurry @@ Endpoints.Api.(href @@ Book Pdf) (Entry.id book) (Book.slug' book)) dialog.parameters_signal) ();
    ]

let create_and_open book = open_ book =<< create ()
