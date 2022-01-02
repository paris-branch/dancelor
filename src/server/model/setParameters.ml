open Nes
include Dancelor_common_model.SetParameters

let for_dance p =
  match for_dance p with
  | None -> Lwt.return_none
  | Some dance_slug ->
    let%lwt dance = Dance.get dance_slug in
    Lwt.return_some dance

let make ?instruments ?forced_pages ?show_deviser ?for_dance ?every_version () =
  let%lwt for_dance =
    match for_dance with
    | None -> Lwt.return_none
    | Some dance -> Dance.slug dance >>=| Lwt.return_some
  in
  Lwt.return (make ?instruments ?forced_pages ?show_deviser ?for_dance ?every_version ())
