open Nes
open Common

open Html

let name_gen tune_gen =
  span [
    match tune_gen with
    | Right (tune, true) ->
      a
        ~a: [a_href @@ Endpoints.Page.href_tune @@ Entry.id tune]
        [txt @@ Model.Tune.one_name' tune]
    | Right (tune, _) -> txt (Model.Tune.one_name' tune)
    | Left tune -> txt (Model.Tune.one_name tune)
  ]

let name = name_gen % Either.left
let name' ?(link = true) tune = name_gen @@ Right (tune, link)

let composers ?short tune =
  with_span_placeholder
    (List.singleton <$> (Person.names' ?short <$> Model.Tune.composers tune))

let composers' ?short tune = composers ?short (Entry.value tune)

let description tune =
  with_span_placeholder @@
    let kind = Kind.Base.to_pretty_string @@ Model.Tune.kind tune in
    match%lwt Model.Tune.composers tune with
    | [] ->
      lwt
        [
          txt (String.capitalize_ascii kind)
        ]
    | [composer] when NEString.to_string (Model.Person.name' composer) = "Traditional" ->
      lwt
        [
          txt ("Traditional " ^ kind)
        ]
    | composers ->
      lwt
        (
          [txt (String.capitalize_ascii kind ^ " by ");
          Person.names' composers;
          ]
        )

let description' = description % Entry.value

let aka tune =
  span @@
    match Model.Tune.other_names tune with
    | [] -> []
    | names -> [txt @@ spf "Also known as %s" @@ String.concat ", " names]

let aka' = aka % Entry.value
