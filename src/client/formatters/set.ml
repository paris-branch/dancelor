open Nes
open Common

open Html

let works set =
  match%lwt Model.Set.dances set with
  | [] -> Lwt.return_nil
  | dances -> Lwt.return [txt (spf "Works for %s" @@ String.concat ", " @@ List.map Model.Dance.name' dances)]

let works' = works % Entry.value

let name_gen = function
  | Right (set, true) ->
    (* true = we want a link *)
    [a
      ~a: [a_href @@ Endpoints.Page.href_set @@ Entry.slug set]
      [txt @@ Model.Set.name' set]]
  | Right (set, _) -> [txt @@ Model.Set.name' set]
  | Left set -> [txt @@ Model.Set.name set]

let name = name_gen % Either.left
let name' ?(link = true) set = name_gen @@ Right (set, link)

let tunes ?link set =
  let%lwt contents = Model.Set.contents set in
  List.map (List.singleton % Version.name' ?link % fst) contents
  |> List.interspersei (fun _ -> [txt " - "])
  |> List.flatten
  |> List.cons (txt "Tunes: ")
  |> span ~a: [a_class ["opacity-50"]]
  |> List.singleton
  |> Lwt.return

let tunes' ?link set = tunes ?link @@ Entry.value set

let name_and_tunes_gen ?tunes_link set =
  let%lwt tunes = tunes ?link: tunes_link @@ Either.fold ~left: Fun.id ~right: (Entry.value % fst) set in
  Lwt.return (name_gen set @ [br (); small tunes])

let name_and_tunes ?tunes_link set = name_and_tunes_gen ?tunes_link @@ Left set
let name_and_tunes' ?(name_link = true) ?tunes_link set = name_and_tunes_gen ?tunes_link @@ Right (set, name_link)

let name_tunes_and_dance_gen ?tunes_link ?dance_link set parameters =
  let%lwt name_and_tunes = name_and_tunes_gen ?tunes_link set in
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

let name_tunes_and_dance ?tunes_link ?dance_link set parameters =
  name_tunes_and_dance_gen ?tunes_link ?dance_link (Left set) parameters

let name_tunes_and_dance' ?(name_link = true) ?tunes_link ?dance_link set parameters =
  name_tunes_and_dance_gen ?tunes_link ?dance_link (Right (set, name_link)) parameters
