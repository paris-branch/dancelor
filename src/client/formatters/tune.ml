open Nes
open Common

open Html

let name_gen tune_gen =
  span [
    match tune_gen with
    | Right (tune, true) ->
      a
        ~a: [a_href @@ Endpoints.Page.href_tune @@ Entry.slug tune]
        [txt @@ Model.Tune.name' tune]
    | Right (tune, _) -> txt (Model.Tune.name' tune)
    | Left tune -> txt (Model.Tune.name tune)
  ]

let name = name_gen % Either.left
let name' ?(link = true) tune = name_gen @@ Right (tune, link)

let composers ?short tune =
  with_span_placeholder @@
  Lwt.map (List.singleton % Person.names' ?short) @@ Model.Tune.composers tune

let composers' ?short tune = composers ?short (Entry.value tune)

let description tune =
  with_span_placeholder @@
    let kind = Kind.Base.to_pretty_string @@ Model.Tune.kind tune in
    match%lwt Model.Tune.composers tune with
    | [] ->
      Lwt.return
        [
          txt (String.capitalize_ascii kind)
        ]
    | [composer] when Model.Person.is_trad' composer ->
      Lwt.return
        [
          txt ("Traditional " ^ kind)
        ]
    | composers ->
      Lwt.return
        (
          [txt (String.capitalize_ascii kind ^ " by ");
          Person.names' composers;
          ]
        )

let description' = description % Entry.value

let aka tune =
  span @@
    match Model.Tune.alternative_names tune with
    | [] -> []
    | names -> [txt @@ spf "Also known as %s" @@ String.concat ", " names]

let aka' = aka % Entry.value
