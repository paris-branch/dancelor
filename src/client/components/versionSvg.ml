open Nes
open Common

open Html

let make_gen href =
  R.div
    ~a: [
      (* On mobile, space gets parse, so we compensate the container's padding
         by a negative margin, making the tune container take almost the whole
         width (effectively hiding the bar numbers; see also below). *)
      a_class ["mx-n2"; "mx-sm-0"];
    ]
    (
      S.from' [div_placeholder ~min: 12 ~max: 20 ()] @@
        let%lwt href = href in
        lwt [
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
    )

let make ?(params = Model.VersionParameters.none) version =
  make_gen @@
    let%lwt slug = Model.Version.slug' version in
    Job.file_href
      slug
      Endpoints.Api.(route @@ Version BuildSvg)
      (Entry.id version)
      params
      RenderingParameters.none

let make_preview ?(params = Model.VersionParameters.none) version =
  make_gen @@
    Job.file_href
      (Entry.Slug.of_string "preview.svg")
      Endpoints.Api.(route @@ Version BuildSvg')
      version
      params
      RenderingParameters.none
