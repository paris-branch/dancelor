open Nes
open Dancelor_common
open Model_new
open Html

let switch_signal_option = function
  | None -> S.Option.none
  | Some signal -> S.Option.some signal

module Person = struct
  let name ?(link = true) ?context (person : Person_name.t) =
    if link then
      a
        ~a: [R.a_href @@ S.map (fun context -> Endpoints.Page.href_person ?context person.id) (switch_signal_option context)]
        [txt person.name]
    else
      txt person.name

  let names ?(short = false) ?links (persons : Person_name.t list) =
    let persons = List.map (name ?link: links) persons in
    if short then
      match persons with
      | [] -> []
      | [p] -> [p]
      | [p; q] -> [p; txt " & "; q]
      | p :: _ -> [p; txt " et al."]
    else
      List.interspersei (fun _ -> txt ", ") ~last: (fun _ -> txt " and ") persons
end

module Source = struct
  let name ?(link = true) ?context (source : Source_row.t) =
    if link then
      a
        ~a: [R.a_href @@ S.map (fun context -> Endpoints.Page.href_source ?context source.id) (switch_signal_option context)]
        [txt source.name]
    else
      txt source.name

  let short_name ?(link = true) ?context (source : Source_short_name.t) =
    if link then
      a
        ~a: [R.a_href @@ S.map (fun context -> Endpoints.Page.href_source ?context source.id) (switch_signal_option context)]
        [txt source.short_name]
    else
      txt source.short_name
end

module Dance = struct
  let name ?(link = true) ?context (dance : Dance_name.t) =
    if link then
      a
        ~a: [R.a_href @@ S.map (fun context -> Endpoints.Page.href_dance ?context dance.id) (switch_signal_option context)]
        [txt dance.name]
    else
      txt dance.name

  let name_and_disambiguation ?link ?context (dance : Dance_row.t) =
    let disambiguation_block =
      match dance.disambiguation with
      | None -> []
      | Some disambiguation -> [span ~a: [a_class ["opacity-50"]] [txtf " (%s)" disambiguation]]
    in
    name ?link ?context {id = dance.id; name = dance.name} :: disambiguation_block
end

module Tune = struct
  let name ?(link = true) ?context (tune : Tune_name.t) =
    if link then
      a
        ~a: [R.a_href @@ S.map (fun context -> Endpoints.Page.href_tune ?context tune.id) (switch_signal_option context)]
        [txt tune.name]
    else
      txt tune.name

  let name_row ?link ?context (tune : Tune_row.t) = name ?link ?context {id = tune.id; name = tune.name}
end

module Version = struct
  let name_row ?(link = true) ?context (version : Version_row.t) =
    if link then
      a
        ~a: [R.a_href @@ S.map (fun context -> Endpoints.Page.href_version ?context version.tune.id (Some version.id)) (switch_signal_option context)]
        [txt version.tune.name]
    else
      txt version.tune.name
end

module Set = struct
  let name ?(link = true) ?context (set : Set_row.t) =
    if link then
      a
        ~a: [R.a_href @@ S.map (fun context -> Endpoints.Page.href_set ?context set.id) (switch_signal_option context)]
        [txt set.name]
    else
      txt set.name
end

module Book = struct
  let name ?(link = true) ?context (book : Book_row.t) =
    if link then
      a
        ~a: [R.a_href @@ S.map (fun context -> Endpoints.Page.href_book ?context book.id) (switch_signal_option context)]
        [txt book.name]
    else
      txt book.name
end
