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
  let%lwt author = M.Tune.author tune in
  match author with
  | None ->
    Lwt.return [
      txt (String.capitalize_ascii kind)
    ]
  | Some author when M.Person.is_trad author ->
    Lwt.return [
      txt ("Traditional " ^ kind)
    ]
  | Some author ->
    let name_block = Person.name ~link:true (Some author) in
    Lwt.return (
      [txt (String.capitalize_ascii kind ^ " by ")]
      @ name_block
    )

let aka tune =
  match M.Tune.alternative_names tune with
  | [] -> []
  | names -> [txt @@ spf "Also known as %s" @@ String.concat ", " names]
