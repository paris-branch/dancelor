open Nes
open Dancelor_common
open Dancelor_client_model
open Dancelor_client_html
open Dancelor_client_components
open Js_of_ocaml

(* REVIEW: This is close to `VersionDownloadDialog.t`; there is room for
   factorisation here. *)
type t =
  {
    choice_rows : Html_types.tr elt list;
    parameters_signal : BookParameters.t option React.signal;
  }

let lift_set_parameters every_set =
  BookParameters.make ~every_set ()

let create () =
  let set_dialog = SetDownloadDialog.create () in

  let (booklet_choices, booklet_choices_signal) =
    Choices.(make [
        choice [txt "Normal"] ~checked:true;

        choice [txt "Booklet"]
          ~value:(BookParameters.make
                    ~front_page:true
                    ~table_of_contents:End
                    ~two_sided:true
                    ~every_set:SetParameters.(make ~forced_pages:2 ())
                    ());
      ])
  in

  {
    choice_rows =
      (
        set_dialog.choice_rows
        @ [
          tr [td [label [txt "Mode:"]]; td [booklet_choices]]
        ]
      );

    parameters_signal =
      S.merge (Option.concat BookParameters.compose) None [
        S.map (Option.map lift_set_parameters) set_dialog.parameters_signal;
        booklet_choices_signal;
      ]
  }

(* REVIEW: This is extremely close to `VersionDownloadDialog.render` (apart for
   one line and one type, really); there is room for factorisation here. *)
let render slug dialog =
  ModalBox.make [
    h2 ~a:[a_class ["title"]] [txt "Download a PDF"];

    form [
      table dialog.choice_rows;

      input
        ~a:[
          a_class ["button"];
          a_input_type `Submit;
          a_value "Download";
          a_onclick (fun _event ->
              let parameters = S.value dialog.parameters_signal in
              let href = ApiRouter.(path @@ bookPdf slug parameters) in
              ignore (Dom_html.window##open_ (Js.string href) (Js.string "_blank") Js.null);
              false
            );
        ] ();
    ];
  ]

let create_and_render slug = render slug (create ())
