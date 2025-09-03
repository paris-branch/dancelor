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

let make_gen ?show_logs ?(show_audio = true) svg_status_signal ogg_status_signal =
  div (
    [div [make_svg_gen ?show_logs svg_status_signal]] @ (
      if show_audio then
          [div ~a: [a_class ["mt-1"; "d-flex"; "justify-content-end"]] [make_ogg_gen ogg_status_signal]]
      else []
    )
  )

let make ?show_logs ?show_audio ?(params = Model.VersionParameters.none) version =
  make_gen
    ?show_logs
    ?show_audio
    (
      Job.status_signal_from_promise @@
        let%lwt slug = Model.Version.slug' version in
        lwt @@
          Job.run
            (Entry.Slug.add_suffix slug ".svg")
            Endpoints.Api.(route @@ Version BuildSvg)
            (Entry.id version)
            params
            RenderingParameters.none
    )
    (
      Job.status_signal_from_promise @@
        let%lwt slug = Model.Version.slug' version in
        lwt @@
          Job.run
            (Entry.Slug.add_suffix slug ".ogg")
            Endpoints.Api.(route @@ Version BuildOgg)
            (Entry.id version)
            params
            RenderingParameters.none
    )

let make_preview ?show_logs ?show_audio ?(params = Model.VersionParameters.none) version =
  make_gen
    ?show_logs
    ?show_audio
    (
      Job.run
        Entry.Slug.(add_suffix (of_string "preview") ".svg")
        Endpoints.Api.(route @@ Version BuildSvg')
        version
        params
        RenderingParameters.none
    )
    (
      Job.run
        Entry.Slug.(add_suffix (of_string "preview") ".ogg")
        Endpoints.Api.(route @@ Version BuildOgg')
        version
        params
        RenderingParameters.none
    )
