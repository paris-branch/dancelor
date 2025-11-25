open Nes
open Common
open Html

let make_svg_gen ?(show_logs = false) status_signal =
  let on_succeeded href = [
    object_
      ~a: [
        (* Tune previews are generated on A4 paper -- 21cm wide -- with a left margin
           of 1cm for page numbers -- that is 5% of the whole page. Since we want page
           numbers to actually appear in the margin on the website, we make the image
           bigger but remove those 5% of margin on the left. *)
        a_style "margin-left: -5%; width: 105%;";
        a_mime_type "image/svg+xml";
        a_data href;
      ]
      []
  ]
  in
  R.div
    ~a: [
      (* On mobile, space gets parse, so we compensate the container's padding
         by a negative margin, making the tune container take almost the whole
         width (effectively hiding the bar numbers; see also below). *)
      a_class ["mx-n2"; "mx-sm-0"];
    ] @@
    (if show_logs then Job.show_live_status else Job.show_placeholder)
      ~on_succeeded
      status_signal

let make_ogg_gen status_signal =
  R.div
    ~a: [
      (* On mobile, space gets parse, so we compensate the container's padding
         by a negative margin, making the tune container take almost the whole
         width (effectively hiding the bar numbers; see also below). *)
      a_class ["mx-n2"; "mx-sm-0"];
    ]
    (
      (* we go via an intermediary signal, so as to avoid the placeholder flickering
         on irrelevant changes of status *)
      flip S.map (S.map Job.status_to_wait_status status_signal) @@ function
        | Waiting -> [audio ~a: [a_controls (); a_class ["placeholder"]] []]
        | Failed -> [audio ~a: [a_controls (); a_class ["bg-danger"; "opacity-50"]] []]
        | Succeeded src -> [audio ~a: [a_controls ()] ~src []]
    )

let make_gen
    ?show_logs
    ?(show_audio = true)
    ?(copyright_reason_promise = lwt_none)
    svg_status_signal
    ogg_status_signal
  =
  div [
    div [make_svg_gen ?show_logs svg_status_signal];
    div ~a: [a_class ["mt-1"; "d-flex"; "justify-content-between"]] [
      (
        with_div_placeholder ~a: [a_class ["alert"; "alert-info"; "me-1"; "lh-sm"]] @@
          match%lwt copyright_reason_promise with
          | None -> lwt []
          | Some reason ->
            List.cons (txt "You may see this version's content because ")
            <$>
              match (reason : Endpoints.Version.Copyright_response.reason) with
              | Connected ->
                lwt [txt "you are connected."]
              | Composer_agrees ->
                lwt [txt "the composer agrees to have his tunes available publicly."]
              | Publisher_agrees source ->
                let%lwt editors = Model.Source.editors' source in
                lwt
                  [
                    txt "it is published in ";
                    Formatters.Source.name' source;
                    txt ", and the publisher, ";
                    Formatters.Person.names' editors;
                    txt ", agrees to have their publications available publicly.";
                  ]
      );
      (if show_audio then make_ogg_gen ogg_status_signal else div []);
    ];
  ]

let make ?show_logs ?show_audio ?(params = Model.VersionParameters.none) version =
  let (copyright_reason_promise, svg_status_signal) =
    let copyright_response_promise =
      Madge_client.call
        Endpoints.Api.(route @@ Version BuildSvg)
        (Entry.id version)
        params
        RenderingParameters.none
    in
    let copyright_reason_promise =
      Job.copyright_reponse_promise_to_copyright_reason_promise
        copyright_response_promise
    in
    let svg_status_signal =
      Job.status_signal_from_promise @@
        let%lwt slug = Model.Version.slug' version in
        lwt @@
        Job.run3 (Entry.Slug.add_suffix slug ".svg") @@
        Job.copyright_reponse_promise_to_job_registration_promise copyright_response_promise
    in
      (copyright_reason_promise, svg_status_signal)
  in
  let ogg_status_signal =
    let copyright_response_promise =
      Madge_client.call
        Endpoints.Api.(route @@ Version BuildOgg)
        (Entry.id version)
        params
        RenderingParameters.none
    in
    let ogg_status_signal =
      Job.status_signal_from_promise @@
        let%lwt slug = Model.Version.slug' version in
        lwt @@
        Job.run3 (Entry.Slug.add_suffix slug ".svg") @@
        Job.copyright_reponse_promise_to_job_registration_promise copyright_response_promise
    in
    ogg_status_signal
  in
  make_gen
    ?show_logs
    ?show_audio
    ~copyright_reason_promise
    svg_status_signal
    ogg_status_signal

let make_preview ?show_logs ?show_audio ?(params = Model.VersionParameters.none) version =
  let svg_status_signal =
    Job.run
      Entry.Slug.(add_suffix (of_string "preview") ".svg")
      Endpoints.Api.(route @@ Version BuildSvg')
      version
      params
      RenderingParameters.none
  in
  let ogg_status_signal =
    Job.run
      Entry.Slug.(add_suffix (of_string "preview") ".ogg")
      Endpoints.Api.(route @@ Version BuildOgg')
      version
      params
      RenderingParameters.none
  in
  make_gen
    ?show_logs
    ?show_audio
    svg_status_signal
    ogg_status_signal
