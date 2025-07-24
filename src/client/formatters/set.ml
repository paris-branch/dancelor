open Nes
open Common

open Html

let works set =
  with_span_placeholder @@
    match%lwt Model.Set.dances set with
    | [] -> lwt_nil
    | dances -> lwt [txt (spf "Works for %s" @@ String.concat ", " @@ List.map Model.Dance.name' dances)]

let works' = works % Entry.value

let name_gen set_gen =
  span [
    match set_gen with
    | Right (set, true) ->
      a
        ~a: [a_href @@ Endpoints.Page.href_set @@ Entry.id set]
        [txt @@ Model.Set.name' set]
    | Right (set, _) -> txt (Model.Set.name' set)
    | Left set -> txt (Model.Set.name set)
  ]

let name = name_gen % Either.left
let name' ?(link = true) set = name_gen @@ Right (set, link)

let tunes ?link set =
  with_span_placeholder @@
    let%lwt contents = Model.Set.contents set in
    List.map (List.singleton % Version.name' ?link % fst) contents
    |> List.interspersei (fun _ -> [txt " - "])
    |> List.flatten
    |> List.cons (txt "Tunes: ")
    |> span ~a: [a_class ["opacity-50"]]
    |> List.singleton
    |> lwt

let tunes' ?link set = tunes ?link @@ Entry.value set

let name_and_tunes_gen ?tunes_link set =
  span [
    name_gen set;
    br ();
    small [tunes ?link: tunes_link @@ Either.fold ~left: Fun.id ~right: (Entry.value % fst) set];
  ]

let name_and_tunes ?tunes_link set = name_and_tunes_gen ?tunes_link @@ Left set
let name_and_tunes' ?(name_link = true) ?tunes_link set = name_and_tunes_gen ?tunes_link @@ Right (set, name_link)

let name_tunes_and_dance_gen ?tunes_link ?dance_link set parameters =
  let dance =
    with_span_placeholder @@
      match%lwt Model.SetParameters.for_dance parameters with
      | None -> lwt_nil
      | Some dance ->
        lwt
          [
            span
              ~a: [a_class ["opacity-50"]]
              [
                txt "For dance: ";
                Dance.name' ?link: dance_link dance;
              ]
          ]
  in
  span [
    name_and_tunes_gen ?tunes_link set;
    br ();
    small [dance];
  ]

let name_tunes_and_dance ?tunes_link ?dance_link set parameters =
  name_tunes_and_dance_gen ?tunes_link ?dance_link (Left set) parameters

let name_tunes_and_dance' ?(name_link = true) ?tunes_link ?dance_link set parameters =
  name_tunes_and_dance_gen ?tunes_link ?dance_link (Right (set, name_link)) parameters

let conceptors ?short tune =
  with_span_placeholder
    (List.singleton <$> (Person.names' ?short <$> Model.Set.conceptors tune))

let conceptors' ?short tune = conceptors ?short (Entry.value tune)
