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
    ?(is_protected_promise = lwt_false)
    svg_status_signal
    ogg_status_signal
  =
  div [
    div ~a: [R.a_class (S.from' [] @@ if%lwt is_protected_promise then lwt ["d-none"] else lwt_nil)] [
      div [make_svg_gen ?show_logs svg_status_signal];
      div ~a: [a_class ["mt-1"; "d-flex"; "justify-content-end"]] [
        (if show_audio then make_ogg_gen ogg_status_signal else div []);
      ];
    ];
    (
      let classes = ["alert"; "alert-warning"] in
      let classes_none = ["alert"; "alert-warning"; "d-none"] in
      div ~a: [R.a_class (S.from' classes_none @@ if%lwt is_protected_promise then lwt classes else lwt classes_none)] [
        txt "You cannot see the content of this version because it is protected, for copyright reasons."
      ]
    );
  ]

let make ?show_logs ?show_audio ?(params = Model.Version_parameters.none) version =
  let copyright_response_promise =
    Madge_client.call
      Endpoints.Api.(route @@ Version BuildSnippets)
      (Entry.id version)
      params
      Rendering_parameters.none
  in
  let is_protected_promise =
    match%lwt copyright_response_promise with
    | Error error -> raise (Madge_client.Error error)
    | Ok Endpoints.Version.Protected -> lwt_true
    | Ok Endpoints.Version.Granted _ -> lwt_false
  in
  let svg_status_signal =
    Job.status_signal_from_promise @@
      let%lwt slug = Model.Version.slug' version in
      lwt @@
      Job.run3 (NesSlug.add_suffix slug ".svg") @@
      Job.copyright_reponse_promise_to_job_registration_promise @@
      match%lwt copyright_response_promise with
      | Error err -> lwt_error err
      | Ok copyright_response ->
        lwt_ok @@
          Endpoints.Version.map_copyright_response
            (function
              | Endpoints.Job.AlreadySucceeded snippet_ids -> Endpoints.Job.AlreadySucceeded snippet_ids.Endpoints.Version.Snippet_ids.svg_job_id
              | Registered snippet_ids -> Registered snippet_ids.Endpoints.Version.Snippet_ids.svg_job_id
            )
            copyright_response
  in
  let ogg_status_signal =
    Job.status_signal_from_promise @@
      let%lwt slug = Model.Version.slug' version in
      lwt @@
      Job.run3 (NesSlug.add_suffix slug ".ogg") @@
      Job.copyright_reponse_promise_to_job_registration_promise @@
      match%lwt copyright_response_promise with
      | Error err -> lwt_error err
      | Ok copyright_response ->
        lwt_ok @@
          Endpoints.Version.map_copyright_response
            (function
              | Endpoints.Job.AlreadySucceeded snippet_ids -> Endpoints.Job.AlreadySucceeded snippet_ids.Endpoints.Version.Snippet_ids.svg_job_id
              | Registered snippet_ids -> Registered snippet_ids.Endpoints.Version.Snippet_ids.ogg_job_id
            )
            copyright_response
  in
  make_gen
    ?show_logs
    ?show_audio
    ~is_protected_promise
    svg_status_signal
    ogg_status_signal

let make_preview ?show_logs ?show_audio ?(params = Model.Version_parameters.none) version =
  let copyright_response_promise =
    Madge_client.call
      Endpoints.Api.(route @@ Version BuildSnippets')
      version
      params
      Rendering_parameters.none
  in
  let svg_status_signal =
    Job.status_signal_from_promise @@
      let%lwt slug = Model.Version.slug version in
      lwt @@
      Job.run3 (NesSlug.add_suffix slug ".svg") @@ (
        Result.to_option
        <$>
          match%lwt copyright_response_promise with
          | Error err -> lwt_error err
          | Ok Endpoints.Job.AlreadySucceeded snippet_ids -> lwt_ok @@ Endpoints.Job.AlreadySucceeded snippet_ids.Endpoints.Version.Snippet_ids.svg_job_id
          | Ok Registered snippet_ids -> lwt_ok @@ Endpoints.Job.Registered snippet_ids.Endpoints.Version.Snippet_ids.svg_job_id
      )
  in
  let ogg_status_signal =
    Job.status_signal_from_promise @@
      let%lwt slug = Model.Version.slug version in
      lwt @@
      Job.run3 (NesSlug.add_suffix slug ".ogg") @@ (
        Result.to_option
        <$>
          match%lwt copyright_response_promise with
          | Error err -> lwt_error err
          | Ok Endpoints.Job.AlreadySucceeded snippet_ids -> lwt_ok @@ Endpoints.Job.AlreadySucceeded snippet_ids.Endpoints.Version.Snippet_ids.ogg_job_id
          | Ok Registered snippet_ids -> lwt_ok @@ Endpoints.Job.Registered snippet_ids.Endpoints.Version.Snippet_ids.ogg_job_id
      )
  in
  make_gen
    ?show_logs
    ?show_audio
    svg_status_signal
    ogg_status_signal
