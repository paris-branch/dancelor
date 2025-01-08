open Nes
open Dancelor_common
open Dancelor_client_model
open Dancelor_client_html
open Dancelor_client_components

(* REVIEW: This is close to `VersionDownloadDialog.t`; there is room for
   factorisation here. *)
type t = {
  choice_rows: Html_types.tr elt list;
  parameters_signal: BookParameters.t option React.signal;
}

let lift_set_parameters every_set =
  BookParameters.make ~every_set ()

let create () =
  let set_dialog = SetDownloadDialog.create () in
  let booklet_choices =
    Choices.(
      make_radios
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
      set_dialog.choice_rows @
      [
        tr [td [label [txt "Mode:"]]; td [Choices.render booklet_choices]]
      ]
    );
    parameters_signal =
      S.merge
        (Option.concat BookParameters.compose)
        None
        [
          S.map (Option.map lift_set_parameters) set_dialog.parameters_signal;
          Choices.signal booklet_choices;
        ]
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
            R.a_href (S.map (fun params -> ApiRouter.(path @@ route @@ Book Pdf) params (Slug.to_string slug)) dialog.parameters_signal);
            a_onclick (fun _ -> return (); true);
          ]
          [txt "Download"];
      ];
  ]

let create_and_open slug = open_ slug (create ())
