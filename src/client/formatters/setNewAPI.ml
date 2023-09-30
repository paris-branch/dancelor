open Nes
open Dancelor_common
open Dancelor_client_html.NewAPI
module M = Dancelor_client_model

let name ?(link=true) set =
  let name_text = [R.txt @@ S.from' "" @@ M.Set.name set] in
  let%lwt is_inline = M.Set.is_slug_none set in
  if link && not is_inline then
    let href =
      let%lwt slug = M.Set.slug set in
      Lwt.return PageRouter.(path (Set slug))
    in
    Lwt.return [a ~a:[R.a_href @@ S.from' "" href] name_text]
  else
    Lwt.return name_text

let name_and_tunes ?link ?tunes_link set =
  let%lwt name = name ?link set in
  let%lwt versions =
    let%lwt versions_and_parameters = M.Set.versions_and_parameters set in
    let%lwt versions =
      Lwt_list.map_p
        (fun (version, _) -> VersionNewAPI.name ?link:tunes_link version)
        versions_and_parameters
    in
    versions
    |> List.intertwine (fun _ -> [txt " - "])
    |> List.flatten
    |> List.cons (txt "Tunes: ")
    |> span ~a:[a_class ["dim"; "details"]]
    |> List.singleton
    |> Lwt.return
  in
  Lwt.return (name @ versions)
