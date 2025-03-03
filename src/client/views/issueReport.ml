open Nes
open Common

open Js_of_ocaml
open Html
open Components

let describe =
  Endpoints.Page.make_describe
    ~get_version: Model.Version.get
    ~get_tune: Model.Tune.get
    ~get_set: Model.Set.get
    ~get_book: Model.Book.get
    ~get_dance: Model.Dance.get
    ~get_person: Model.Person.get

let open_dialog page =
  let (has_interacted, set_interacted) = S.create false in
  let set_interacted () = set_interacted true in
  let reporter_input =
    Input.Text.make ~has_interacted "" @@
    Result.of_string_nonempty ~empty: "You must specify the reporter"
  in
  let%lwt source =
    Fun.flip Lwt.map (describe page) @@ function
    | None ->
      Choices.make_radios'
        ~name: "Source of the issue"
        ~has_interacted
        ~validate: (Option.to_result ~none: "You must make a choice")
        [
          Choices.choice' ~value: true [txt "Dancelor itself"] ~checked: true;
        ]
    | Some (kind, name) ->
      Choices.make_radios'
        ~name: "Source of the issue"
        ~has_interacted
        ~validate: (Option.to_result ~none: "You must make a choice")
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
    Input.Text.make ~has_interacted "" @@
    Result.of_string_nonempty ~empty: "The title cannot be empty"
  in
  let description_input =
    Input.Text.make ~has_interacted "" @@
    Result.of_string_nonempty ~empty: "The description cannot be empty"
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
    Dialog.open_res @@ fun return ->
    Page.make
      ~title: (S.const "Report an issue")
      [
        Input.Text.render
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
        Button.make
          ~label: "Report"
          ~label_processing: "Reporting..."
          ~icon: "bug_report"
          ~classes: ["btn-success"]
          ~disabled: (S.map Option.is_none request_signal)
          ~onclick: (fun () ->
              set_interacted ();
              Option.fold
                (S.value request_signal)
                ~none: Lwt.return_unit
                ~some: (fun request ->
                    let%lwt response = Madge_cohttp_lwt_client.call Endpoints.Api.(route ReportIssue) request in
                    return @@ Ok response;
                    Lwt.return_unit
                  )
            )
          ();
        Button.cancel ~return ()
      ]
  in
  Dialog.open_ @@ fun return ->
  Page.make
    ~title: (S.const (match response with Ok _ -> "Issue reported" | Error _ -> "Error reporting issue"))
    [
      div
        (
          match response with
          | Ok response ->
            [
              p [txt "Your issue has been reported as:"];
              p [a ~a: [a_href response.uri; a_target "_blank"] [txt @@ spf "%s (#%d)" response.title response.id]];
              p [txt "You can track its progress there."];
            ]
          | Error _ -> [p [txt "There was an error reporting the issue. Please contact your system administrator because this is really not supposed to happen."]]
        );
    ]
    ~buttons: [
      Button.make
        ~label: "Ok"
        ~label_processing: "Closing..."
        ~icon: "check_circle"
        ~classes: ["btn-success"]
        ~onclick: (fun () -> return (Ok ()); Lwt.return_unit)
        ()
    ]

let get_uri () = Uri.of_string (Js.to_string Dom_html.window##.location##.href)

let button =
  div
    ~a: [a_id "issue-report-button"]
    [
      a
        ~a: [
          a_onclick (fun _ ->
              Lwt.async (fun () ->
                  Lwt.map ignore @@ open_dialog @@ get_uri ()
                );
              false
            );
        ]
        [
          i ~a: [a_class ["material-symbols-outlined"]] [txt "bug_report"];
          span [txt " Report an issue"];
        ];
    ]
