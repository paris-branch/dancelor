open Nes
open Common

open Html

let make_gen src =
  R.div
    ~a: [
      (* On mobile, space gets parse, so we compensate the container's padding
         by a negative margin, making the tune container take almost the whole
         width (effectively hiding the bar numbers; see also below). *)
      a_class ["mx-n2"; "mx-sm-0"];
    ]
    (
      S.from' [] @@
        let%lwt src = src in
        lwt [audio ~a: [a_controls ()] ~src []]
    )

let make ?(params = Model.VersionParameters.none) version =
  make_gen @@
    let%lwt slug = Model.Version.slug' version in
    Job.file_href
      slug
      Endpoints.Api.(route @@ Version BuildOgg)
      (Entry.id version)
      params
      RenderingParameters.none

let make_preview ?(params = Model.VersionParameters.none) version =
  make_gen @@
    Job.file_href
      (Entry.Slug.of_string "preview.ogg")
      Endpoints.Api.(route @@ Version BuildOgg')
      version
      params
      RenderingParameters.none
