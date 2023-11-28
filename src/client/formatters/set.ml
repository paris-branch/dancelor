open Nes
open Dancelor_common
open Dancelor_client_html
module M = Dancelor_client_model

let works set =
  match%lwt M.Set.dances set with
  | [] -> Lwt.return_nil
  | dances -> Lwt.return [txt (spf "Works for %s" @@ String.concat ", " @@ List.map M.Dance.name dances)]

let name ?(link=true) set =
  let name_text = [txt (M.Set.name set)] in
  if link && not (M.Set.is_slug_none set) then
    [
      a
        ~a:[a_href @@ PageRouter.path_set @@ M.Set.slug set]
        name_text
    ]
  else
    name_text

let name_and_tunes ?link ?tunes_link set =
  let%lwt versions =
    let%lwt versions_and_parameters = M.Set.versions_and_parameters set in
    List.map (Version.name ?link:tunes_link % fst) versions_and_parameters
    |> List.intertwine (fun _ -> [txt " - "])
    |> List.flatten
    |> List.cons (txt "Tunes: ")
    |> span ~a:[a_class ["dim"; "details"]]
    |> List.singleton
    |> Lwt.return
  in
  Lwt.return (name ?link set @ versions)

let name_tunes_and_dance ?link ?tunes_link ?dance_link set parameters =
  let%lwt name_and_tunes = name_and_tunes ?link ?tunes_link set in
  let%lwt dance =
    match%lwt M.SetParameters.for_dance parameters with
    | None -> Lwt.return_nil
    | Some dance -> Lwt.return [
        span ~a:[a_class ["dim"; "details"]] [
          txt "For dance: ";
          span (Dance.name ?link:dance_link dance)
        ]
      ]
  in
  Lwt.return (name_and_tunes @ dance)
