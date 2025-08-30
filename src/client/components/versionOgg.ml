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
      S.from' [audio ~a: [a_controls (); a_class ["placeholder"]] []] @@
        let%lwt src = src in
        lwt [audio ~a: [a_controls ()] ~src []]
    )

let make ?(params = Model.VersionParameters.none) version =
  make_gen (
    let%lwt slug = Model.Version.slug' version in
    Result.get_ok
    <$> Job.file_href
        slug
        Endpoints.Api.(route @@ Version BuildOgg)
        (Entry.id version)
        params
        RenderingParameters.none
  )

let make_preview ?(params = Model.VersionParameters.none) version =
  make_gen (
    Result.get_ok
    <$> Job.file_href
        (Entry.Slug.of_string "preview.ogg")
        Endpoints.Api.(route @@ Version BuildOgg')
        version
        params
        RenderingParameters.none
  )
