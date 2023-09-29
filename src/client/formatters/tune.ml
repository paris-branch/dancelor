open Nes
open Dancelor_common
open Dancelor_client_html

module M = Dancelor_client_model

let name ?(link=true) tune =
  let name_text = [text lwt (M.Tune.name tune)] in
  if link then
    let href_lwt =
      let%lwt slug = M.Tune.slug tune in
      Lwt.return PageRouter.(path (Tune slug))
    in
    Lwt.return [a ~href_lwt const name_text]
  else
    Lwt.return name_text

let description tune =
  let%lwt kind = M.Tune.kind tune in
  let kind = M.Kind.Base.to_pretty_string kind in
  let%lwt author = M.Tune.author tune in
  match author with
  | None ->
    Lwt.return [
      text const (String.capitalize_ascii kind)
    ]
  | Some author when M.Credit.is_trad author ->
    Lwt.return [
      text const ("Traditional " ^ kind)
    ]
  | Some author ->
    let%lwt line_block = Credit.line ~link:true (Some author) in
    Lwt.return (
      [text const (String.capitalize_ascii kind ^ " by ")]
      @ line_block
    )

let aka tune =
  match%lwt M.Tune.alternative_names tune with
  | [] -> Lwt.return_nil
  | names ->
    Lwt.return [
      text const (spf "Also known as %s" (String.concat ", " names))
    ]

let recommended tune =
  match%lwt M.Tune.dances tune with
  | [] -> Lwt.return_nil
  | dances ->
    let%lwt dance_names = Lwt_list.map_p M.Dance.name dances in
    Lwt.return [
      text const (spf "Recommended for %s" (String.concat ", " dance_names))
    ]
