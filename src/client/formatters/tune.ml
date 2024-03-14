open Nes
open Dancelor_common
open Dancelor_client_html
module M = Dancelor_client_model

let name ?(link=true) tune =
  let name_text = [txt @@ M.Tune.name tune] in
  if link then
    [
      a
        ~a:[a_href @@ PageRouter.path_tune @@ M.Tune.slug tune]
        name_text
    ]
  else
    name_text

let description tune =
  let kind = M.Kind.Base.to_pretty_string @@ M.Tune.kind tune in
  let%lwt composer = M.Tune.composer tune in
  match composer with
  | None ->
    Lwt.return [
      txt (String.capitalize_ascii kind)
    ]
  | Some composer when M.Person.is_trad composer ->
    Lwt.return [
      txt ("Traditional " ^ kind)
    ]
  | Some composer ->
    let name_block = Person.name ~link:true (Some composer) in
    Lwt.return (
      [txt (String.capitalize_ascii kind ^ " by ")]
      @ name_block
    )

let aka tune =
  match M.Tune.alternative_names tune with
  | [] -> []
  | names -> [txt @@ spf "Also known as %s" @@ String.concat ", " names]
