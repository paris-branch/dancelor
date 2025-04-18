open Nes
open Common

open Html

let works set =
  match%lwt Model.Set.dances set with
  | [] -> Lwt.return_nil
  | dances -> Lwt.return [txt (spf "Works for %s" @@ String.concat ", " @@ List.map Model.Dance.name dances)]

let name ?(link = true) set =
  let name_text = [txt (Model.Set.name set)] in
  if link && not (Entry.is_dummy set) then
    [
      a
        ~a: [a_href @@ Endpoints.Page.href_set @@ Entry.slug set]
        name_text
    ]
  else
    name_text

let tunes ?tunes_link set =
  let%lwt contents = Model.Set.contents set in
  List.map (Version.name ?link: tunes_link % fst) contents
  |> List.interspersei (fun _ -> [txt " - "])
  |> List.flatten
  |> List.cons (txt "Tunes: ")
  |> span ~a: [a_class ["opacity-50"]]
  |> List.singleton
  |> Lwt.return

let name_and_tunes ?link ?tunes_link set =
  let%lwt tunes = tunes ?tunes_link set in
  Lwt.return (name ?link set @ [br (); small tunes])

let name_tunes_and_dance ?link ?tunes_link ?dance_link set parameters =
  let%lwt name_and_tunes = name_and_tunes ?link ?tunes_link set in
  let%lwt dance =
    match%lwt Model.SetParameters.for_dance parameters with
    | None -> Lwt.return_nil
    | Some dance ->
      Lwt.return
        [
          span
            ~a: [a_class ["opacity-50"]]
            [
              txt "For dance: ";
              span (Dance.name ?link: dance_link dance)
            ]
        ]
  in
  Lwt.return (name_and_tunes @ [br (); small dance])
