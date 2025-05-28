open Common

open Html

let make_gen href =
  let (loaded, set_loaded) = S.create_oneshot_switch false in
  div
    ~a: [
      (* On mobile, space gets parse, so we compensate the container's padding
         by a negative margin, making the tune container take almost the whole
         width (effectively hiding the bar numbers; see also below). *)
      a_class ["mx-n2"; "mx-sm-0"];
    ]
    [
      div
        ~a: [R.a_class (S.map (function false -> [] | true -> ["d-none"]) loaded)]
        [div_placeholder ~min: 12 ~max: 20 ()];
      object_
        ~a: [
          (* Tune previews are generated on A4 paper -- 21cm wide -- with a left margin
             of 1cm for page numbers -- that is 5% of the whole page. Since we want page
             numbers to actually appear in the margin on the website, we make the image
             bigger but remove those 5% of margin on the left. *)
          a_style "margin-left: -5%; width: 105%;";
          a_mime_type "image/svg+xml";
          a_data href;
          R.a_class (S.map (function false -> ["d-none"] | true -> []) loaded);
          a_onload (fun _ev -> set_loaded (); false);
        ]
        [];
    ]

let make slug =
  make_gen (Endpoints.Api.(href @@ Version Svg) slug Model.VersionParameters.none RenderingParameters.none)

let make_preview version =
  make_gen (Endpoints.Api.(href @@ Version PreviewSvg) version Model.VersionParameters.none RenderingParameters.none)
