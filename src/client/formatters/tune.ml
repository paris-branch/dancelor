open Nes
open Dancelor_common
open Dancelor_client_html
module M = Dancelor_client_model

let name ?(link=true) tune =
  let name_text = [L.txt (M.Tune.name tune)] in
  if link then
    let href =
      let%lwt slug = M.Tune.slug tune in
      Lwt.return PageRouter.(path (Tune slug))
    in
    Lwt.return [a ~a:[L.a_href href] name_text]
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
  match%lwt M.Tune.alternative_names tune with
  | [] -> Lwt.return_nil
  | names ->
    Lwt.return [
      txt (spf "Also known as %s" (String.concat ", " names))
    ]
