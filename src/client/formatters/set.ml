open Nes
open Dancelor_common
open Html

let switch_signal_option = function
  | None -> S.Option.none
  | Some signal -> S.Option.some signal

let display_name ?(params = Model.Set_parameters.none) () =
  match Model.Set_parameters.display_name params with
  | None -> []
  | Some display_name -> [txt " [as “"; txt (NEString.to_string display_name); txt "”]"]

let display_conceptor ?(params = Model.Set_parameters.none) () =
  match Model.Set_parameters.display_conceptor params with
  | None -> []
  | Some display_conceptor -> [txt " [as “"; txt (NEString.to_string display_conceptor); txt "”]"]

let name_gen ?params set_gen =
  span (
    [
      match set_gen with
      | Right (set, true, in_search) ->
        a
          ~a: [R.a_href @@ S.map (fun in_search -> Endpoints.Page.href_set ?in_search @@ Entry.id set) (switch_signal_option in_search)]
          [txt @@ NEString.to_string @@ Model.Set.name' set]
      | Right (set, _, _) -> txt (NEString.to_string @@ Model.Set.name' set)
      | Left set -> txt (NEString.to_string @@ Model.Set.name set)
    ] @
      display_name ?params ()
  )

let name = name_gen % Either.left

let name' ?(link = true) ?params ?in_search set =
  name_gen ?params @@ Right (set, link, in_search)

let tunes ?link set =
  with_span_placeholder @@
    let%lwt versions = Lwt_list.map_p (Option.get <%> Model.Version.get % fst) (Model.Set.contents set) in
    List.map (List.singleton % Version.name' ?link) versions
    |> List.interspersei (fun _ -> [txt " - "])
    |> List.flatten
    |> List.cons (txt "Tunes: ")
    |> span ~a: [a_class ["opacity-50"]]
    |> List.singleton
    |> lwt

let tunes' ?link set = tunes ?link @@ Entry.value set

let conceptors ?link ?short ?params tune =
  span (
    [
      with_span_placeholder
        (List.singleton <$> (Person.names' ?links: link ?short <$> Lwt_list.map_p (Option.get <%> Model.Person.get) (Model.Set.conceptors tune)))
    ] @
      display_conceptor ?params ()
  )

let conceptors' ?link ?short ?params tune =
  conceptors ?link ?short ?params (Entry.value tune)
