open Nes
open Common
open Html

let switch_signal_option = function
  | None -> S.Option.none
  | Some signal -> S.Option.some signal

let name_gen dance_gen =
  span [
    match dance_gen with
    | Right (dance, true, context) ->
      a
        ~a: [R.a_href @@ S.map (fun context -> Endpoints.Page.href_dance ?context @@ Entry.id dance) (switch_signal_option context)]
        [txt @@ NEString.to_string @@ Model.Dance.one_name' dance]
    | Right (dance, _, _) -> txt (NEString.to_string @@ Model.Dance.one_name' dance)
    | Left dance -> txt (NEString.to_string @@ Model.Dance.one_name dance)
  ]

let name = name_gen % Either.left

let name' ?(link = true) ?context person = name_gen @@ Right (person, link, context)

let name_and_disambiguation_gen dance =
  let disambiguation_block =
    match Model.Dance.disambiguation @@ Either.fold ~left: Fun.id ~right: (Entry.value % (fun (d, _, _) -> d)) dance with
    | "" -> []
    | disambiguation -> [span ~a: [a_class ["opacity-50"]] [txt (spf " (%s)" disambiguation)]]
  in
  span (name_gen dance :: disambiguation_block)

let name_and_disambiguation' ?(name_link = true) ?context dance =
  name_and_disambiguation_gen @@ Right (dance, name_link, context)

let aka dance =
  span @@
    match Model.Dance.other_names dance with
    | [] -> []
    | names -> [txt @@ spf "Also known as %s" @@ String.concat ", " @@ List.map NEString.to_string names]

let aka' = aka % Entry.value
