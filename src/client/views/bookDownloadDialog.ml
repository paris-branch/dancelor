open Nes
open Common

open Model
open Html
open Components

(* REVIEW: This is close to `VersionDownloadDialog.t`; there is room for
   factorisation here. *)
type t = {
  choice_rows: Html_types.tr elt list;
  parameters_signal: BookParameters.t React.signal;
}

let lift_set_parameters every_set =
  BookParameters.make ~every_set ()

let create () =
  let set_dialog = SetDownloadDialog.create () in
  let booklet_choices =
    Choices.(
      make_radios
        ~label: "Mode"
        [
          choice' [txt "Normal"] ~checked: true;
          choice'
            [txt "Booklet"]
            ~value: (
              BookParameters.make
                ~front_page: true
                ~table_of_contents: End
                ~two_sided: true
                ~every_set: SetParameters.(make ~forced_pages: 2 ())
                ()
            );
        ]
    )
  in
  {
    choice_rows = (
      set_dialog.choice_rows @ [
        tr [td [label [txt "Mode:"]]; td [Component.inner_html booklet_choices]]
      ]
    );
    parameters_signal = S.map (Option.value ~default: BookParameters.none) @@
      S.merge
        (Option.concat BookParameters.compose)
        None
        [
          S.map (some % lift_set_parameters) set_dialog.parameters_signal;
          S.map Result.get_ok (Component.signal booklet_choices);
        ]
  }

(* REVIEW: This is extremely close to `VersionDownloadDialog.render` (apart for
   one line and one type, really); there is room for factorisation here. *)
let open_ book dialog =
  Page.open_dialog @@ fun return ->
  Page.make'
    ~title: (lwt "Download a PDF")
    [table dialog.choice_rows]
    ~buttons: [
      Utils.Button.cancel' ~return ();
      Utils.Button.download ~href: (S.map (fun params -> Endpoints.Api.(href @@ Book Pdf) (Entry.id book) (Book.slug' book) params RenderingParameters.none) dialog.parameters_signal) ();
    ]

let create_and_open book = open_ book (create ())
