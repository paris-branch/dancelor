open Nes
open Dancelor_common
open Dancelor_client_html
module M = Dancelor_client_model

let name ?(link = true) tune =
  let name_text = [txt @@ M.Tune.name tune] in
  if link then
    [
      a
        ~a: [a_href @@ PageRouter.path_tune @@ M.Tune.slug tune]
        name_text
    ]
  else
    name_text

let composers ?short = Lwt.map (Person.names ?short) % M.Tune.composers

let description tune =
  let kind = M.Kind.Base.to_pretty_string @@ M.Tune.kind tune in
  match%lwt M.Tune.composers tune with
  | [] ->
    Lwt.return
      [
        txt (String.capitalize_ascii kind)
      ]
  | [composer] when M.Person.is_trad composer ->
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
  match M.Tune.alternative_names tune with
  | [] -> []
  | names -> [txt @@ spf "Also known as %s" @@ String.concat ", " names]
