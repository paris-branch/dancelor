open Nes
open Common
open Html

let switch_signal_option = function
  | None -> S.Option.none
  | Some signal -> S.Option.some signal

let name_gen tune_gen =
  span [
    match tune_gen with
    | Right (tune, true, context) ->
      a
        ~a: [R.a_href @@ S.map (fun context -> Endpoints.Page.href_tune ?context @@ Entry.id tune) (switch_signal_option context)]
        [txt @@ NEString.to_string @@ Model.Tune.one_name' tune]
    | Right (tune, _, _) -> txt (NEString.to_string @@ Model.Tune.one_name' tune)
    | Left tune -> txt (NEString.to_string @@ Model.Tune.one_name tune)
  ]

let name = name_gen % Either.left
let name' ?(link = true) ?context tune = name_gen @@ Right (tune, link, context)

let composers ?short ?links tune =
  with_span_placeholder
    (List.singleton <$> ((Person.names' ?short ?links % List.map Model.Tune.composer_composer) <$> Model.Tune.composers tune))

let composers' ?short ?links tune = composers ?short ?links (Entry.value tune)

let description tune =
  with_span_placeholder @@
    let kind = Kind.Base.to_pretty_string @@ Model.Tune.kind tune in
    match%lwt Model.Tune.composers tune with
    | [] ->
      lwt [txt (String.capitalize_ascii kind)]
    | [composer] when NEString.to_string (Model.Person.name' @@ Model.Tune.composer_composer composer) = "Traditional" ->
      lwt [txt ("Traditional " ^ kind)]
    | composers ->
      lwt [
        txt (String.capitalize_ascii kind ^ " by ");
        Person.names'_with_details @@
          List.map
            (fun Model.Tune.{composer; details} ->
              (
                composer,
                if details = "" then [] else [txtf " (%s)" details]
              )
            )
            composers;
      ]

let description' = description % Entry.value

let aka tune =
  span @@
    match Model.Tune.other_names tune with
    | [] -> []
    | names -> [txt @@ spf "Also known as %s" @@ String.concat ", " @@ List.map NEString.to_string names]

let aka' = aka % Entry.value
