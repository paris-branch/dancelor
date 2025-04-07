open Common

open Html

let make_gen href =
  object_
    ~a: [
      (* Tune previews are generated on A4 paper -- 21cm wide -- with a left margin
         of 1cm for page numbers -- that is 5% of the whole page. Since we want page
         numbers to actually appear in the margin on the website, we make the image
         bigger but remove those 5% of margin on the left. *)
      a_style " margin-left: -5%; max-width: 105%;";
      a_mime_type "image/svg+xml";
      a_data href;
    ]
    []

let make slug =
  make_gen (Endpoints.Api.(href @@ Version Svg) Model.VersionParameters.none slug)

let make_preview version =
  make_gen (Endpoints.Api.(href @@ Version PreviewSvg) Model.VersionParameters.none version)
