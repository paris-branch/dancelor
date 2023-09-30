open Nes
open Dancelor_common
open Dancelor_client_html.NewAPI
module M = Dancelor_client_model

let name ?(link=true) tune =
  let name_text = [R.txt @@ S.from' "" @@ M.Tune.name tune] in
  if link then
    let href =
      let%lwt slug = M.Tune.slug tune in
      Lwt.return PageRouter.(path (Tune slug))
    in
    Lwt.return [a ~a:[R.a_href @@ S.from' "" href] name_text]
  else
    Lwt.return name_text

let description tune =
  let%lwt kind = M.Tune.kind tune in
  let kind = M.Kind.Base.to_pretty_string kind in
  let%lwt author = M.Tune.author tune in
  match author with
  | None ->
    Lwt.return [
      txt (String.capitalize_ascii kind)
    ]
  | Some author when M.Credit.is_trad author ->
    Lwt.return [
      txt ("Traditional " ^ kind)
    ]
  | Some author ->
    let%lwt line_block = CreditNewAPI.line ~link:true (Some author) in
    Lwt.return (
      [txt (String.capitalize_ascii kind ^ " by ")]
      @ line_block
    )

let aka tune =
  match%lwt M.Tune.alternative_names tune with
  | [] -> Lwt.return_nil
  | names ->
    Lwt.return [
      txt (spf "Also known as %s" (String.concat ", " names))
    ]