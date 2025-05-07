open Nes
open Common

open Html
open Components

include Endpoints.Page.MakeDescribe(Model.Book)(Model.Dance)(Model.Person)(Model.Set)(Model.Source)(Model.Tune)(Model.Version)

let open_dialog page =
  let reporter_input =
    Input.Text.make "" @@
      Result.of_string_nonempty ~empty: "You must specify the reporter."
  in
  let%lwt source =
    Fun.flip Lwt.map (describe page) @@ function
      | None ->
        Choices.make_radios'
          ~name: "Source of the issue"
          ~validate: (Option.to_result ~none: "You must make a choice.")
          [
            Choices.choice' ~value: true [txt "Dancelor itself"] ~checked: true;
          ]
      | Some (kind, name) ->
        Choices.make_radios'
          ~name: "Source of the issue"
          ~validate: (Option.to_result ~none: "You must make a choice.")
          [
            Choices.choice'
              ~value: false
              [
                txt @@
                  spf "This %s: %s" kind name
              ];
            Choices.choice' ~value: true [txt "Dancelor itself"];
          ]
  in
  let title_input =
    Input.Text.make "" @@
      Result.of_string_nonempty ~empty: "The title cannot be empty."
  in
  let description_input =
    Input.Text.make "" @@
      Result.of_string_nonempty ~empty: "The description cannot be empty."
  in
  let request_signal =
    let page = Uri.to_string page in
    S.map Result.to_option @@
    RS.bind (Input.Text.signal reporter_input) @@ fun reporter ->
    RS.bind (Input.Text.signal title_input) @@ fun title ->
    RS.bind (Input.Text.signal description_input) @@ fun description ->
    RS.bind (Choices.signal source) @@ fun source ->
    RS.pure Endpoints.IssueReport.Request.{reporter; page; source_is_dancelor = source; title; description}
  in
  let%lwt response =
    Page.open_dialog @@ fun return ->
    Page.make
      ~title: (S.const "Report an issue")
      [Input.Text.render
        reporter_input
        ~placeholder: "Dr Jean Milligan"
        ~label: "Reporter";
      Choices.render source;
      Input.Text.render
        title_input
        ~placeholder: "Blimey, 'tis not working!"
        ~label: "Title";
      Input.Text.render_as_textarea
        description_input
        ~placeholder: "I am gutted; this knock off tune is wonky at best!"
        ~label: "Description";
      ]
      ~buttons: [
        Button.cancel' ~return ();
        Button.make
          ~label: "Report"
          ~label_processing: "Reporting..."
          ~icon: "bug"
          ~classes: ["btn-primary"]
          ~disabled: (S.map Option.is_none request_signal)
          ~onclick: (fun () ->
            Option.fold
              (S.value request_signal)
              ~none: Lwt.return_unit
              ~some: (fun request ->
                let%lwt response = Madge_client.call_exn Endpoints.Api.(route ReportIssue) request in
                return @@ Some response;
                Lwt.return_unit
              )
          )
          ();
      ]
  in
  (
    match response with
    | Some response ->
      Components.Toast.open_
        ~title: "Issue reported"
        [
          txt "Your issue has been reported as: ";
          a ~a: [a_href response.uri; a_target "_blank"] [txt @@ spf "%s (#%d)" response.title response.id];
          txt " You can track its progress there.";
        ]
    | None ->
      Components.Toast.open_
        ~title: "Issue not reported"
        [txt "Your issue has not been reported, as you closed the dialog. If this is an error, please contact your system administrator."]
  );
  Lwt.return_unit
