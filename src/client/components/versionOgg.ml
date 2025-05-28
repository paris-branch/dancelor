open Common

open Html

let make_gen href =
  let (loaded, set_loaded) = S.create_oneshot_switch false in
  audio
    ~a: [
      a_controls ();
      R.a_class (S.map (function false -> ["placeholder"] | true -> []) loaded);
      a_onloadeddata (fun _ev -> set_loaded (); false);
    ]
    ~src: href
    []

let make slug =
  make_gen (Endpoints.Api.(href @@ Version Ogg) slug Model.VersionParameters.none RenderingParameters.none)

let make_preview version =
  make_gen (Endpoints.Api.(href @@ Version PreviewOgg) version Model.VersionParameters.none RenderingParameters.none)
