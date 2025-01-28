open Nes
open Html
module PageRouter = Dancelor_common.PageRouter
module Database = Dancelor_common.Database

let name ?(link = true) tune =
  let name_text = [txt @@ Model.Tune.name tune] in
  if link then
    [
      a
        ~a: [a_href @@ PageRouter.href_tune @@ Database.Entry.slug tune]
        name_text
    ]
  else
    name_text

let composers ?short = Lwt.map (Person.names ?short) % Model.Tune.composers

let description tune =
  let kind = Dancelor_common.Kind.Base.to_pretty_string @@ Model.Tune.kind tune in
  match%lwt Model.Tune.composers tune with
  | [] ->
    Lwt.return
      [
        txt (String.capitalize_ascii kind)
      ]
  | [composer] when Model.Person.is_trad composer ->
    Lwt.return
      [
        txt ("Traditional " ^ kind)
      ]
  | composers ->
    Lwt.return
      (
        [txt (String.capitalize_ascii kind ^ " by ")] @
        (Person.names composers)
      )

let aka tune =
  match Model.Tune.alternative_names tune with
  | [] -> []
  | names -> [txt @@ spf "Also known as %s" @@ String.concat ", " names]
