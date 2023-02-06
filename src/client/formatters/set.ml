open Nes
open Dancelor_client_html

module M = Dancelor_client_model
module Router = Dancelor_common.Router

let works set =
  match%lwt M.Set.dances set with
  | [] -> Lwt.return_nil
  | dances ->
    let%lwt dance_names = Lwt_list.map_p M.Dance.name dances in
    Lwt.return
      [
        text (spf "Works for %s" (String.concat ", " dance_names));
      ]

let name ?(link = true) set =
  let name_text = [text_lwt (M.Set.name set)] in
  let%lwt is_inline = M.Set.is_slug_none set in
  if link && not is_inline then
    let href_lwt =
      let%lwt slug = M.Set.slug set in
      Lwt.return (Router.path_of_controller (Router.Set slug) |> snd)
    in
    Lwt.return [a ~href_lwt name_text]
  else
    Lwt.return name_text

let name_and_tunes ?link ?tunes_link set =
  let%lwt name = name ?link set in
  let%lwt versions =
    let%lwt versions_and_parameters = M.Set.versions_and_parameters set in
    let%lwt versions =
      Lwt_list.map_p
        (fun (version, _) -> Version.name ?link: tunes_link version)
        versions_and_parameters
    in
    versions
    |> List.intertwine (fun _ -> [text " - "])
    |> List.flatten
    |> List.cons (text "Tunes: ")
    |> span ~classes: ["dim"; "details"]
    |> List.singleton
    |> Lwt.return
  in
  Lwt.return (name @ versions)

let name_tunes_and_dance ?link ?tunes_link ?dance_link set parameters =
  let%lwt name_and_tunes = name_and_tunes ?link ?tunes_link set in
  let%lwt dance =
    match%lwt M.SetParameters.for_dance parameters with
    | None -> Lwt.return_nil
    | Some dance ->
      Lwt.return
        [
          span
            ~classes: ["dim"; "details"]
            [
              text "For dance: ";
              span_lwt (Dance.name ?link: dance_link dance);
            ];
        ]
  in
  Lwt.return (name_and_tunes @ dance)
