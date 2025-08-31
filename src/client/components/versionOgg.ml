open Nes
open Common

open Html

let make_gen status_signal =
  R.div
    ~a: [
      (* On mobile, space gets parse, so we compensate the container's padding
         by a negative margin, making the tune container take almost the whole
         width (effectively hiding the bar numbers; see also below). *)
      a_class ["mx-n2"; "mx-sm-0"];
    ]
    (
      flip S.map status_signal @@ function
        | Job.Registering | Pending | Running _ ->
          [audio ~a: [a_controls (); a_class ["placeholder"]] []]
        | Failed _ ->
          (* FIXME: visual confirmation that it failed *)
          [audio ~a: [a_controls (); a_class ["placeholder"]] []]
        | Succeeded src ->
          [audio ~a: [a_controls ()] ~src []]
    )

let make ?(params = Model.VersionParameters.none) version =
  make_gen @@
  Job.status_signal_from_promise @@
  let%lwt slug = Model.Version.slug' version in
  Job.run
    (Entry.Slug.add_suffix slug ".ogg")
    Endpoints.Api.(route @@ Version BuildOgg)
    (Entry.id version)
    params
    RenderingParameters.none

let make_preview ?(params = Model.VersionParameters.none) version =
  make_gen @@
  Job.status_signal_from_promise @@
  Job.run
    Entry.Slug.(add_suffix (of_string "preview") ".ogg")
    Endpoints.Api.(route @@ Version BuildOgg')
    version
    params
    RenderingParameters.none
