open Nes
open Js_of_ocaml
module M = Dancelor_client_model

module Html = Dom_html

let js = Js.string


module Kind = struct

  let full_string version tune =
    let open Lwt in
    let%lwt base = M.Tune.kind tune >|= M.Kind.base_to_char in
    let%lwt bars = M.Version.bars version in
    Lwt.return (Printf.sprintf "%i %c" bars base)

  let full_string_lwt version tune =
    let%lwt version = version in
    let%lwt tune = tune in
    full_string version tune

end

module Credit = struct

  let line = function
    | None -> Lwt.return ""
    | Some c -> M.Credit.line c

  let line_lwt credit =
    Lwt.bind credit line

end

module Tune = struct

  let description tune =
    (* FIXME: make author a link *)
    let%lwt kind = M.Tune.kind tune in
    let kind = M.Kind.base_to_pretty_string kind in
    let%lwt author = M.Tune.author tune in
    match author with
    | None ->
      Lwt.return (String.capitalize_ascii kind)
    | Some author when M.Credit.is_trad author ->
      Lwt.return ("Traditional " ^ kind)
    | Some author ->
      let%lwt line = M.Credit.line author in
      Lwt.return (String.capitalize_ascii kind ^ " by " ^ line)

  let description_lwt tune =
    Lwt.bind tune description

  let aka tune =
    match%lwt M.Tune.alt_names tune with
    | [] -> Lwt.return ""
    | names -> Printf.sprintf "Also known as: %s" (String.concat ", " names) |> Lwt.return

  let aka_lwt tune =
    Lwt.bind tune aka

end

module Version = struct

  let description version =
    (* FIXME: make arranger a link *)
    let%lwt bars = M.Version.bars version in
    let%lwt structure = M.Version.structure version in
    let%lwt key = M.Version.key version in
    let%lwt arranger =
      match%lwt M.Version.arranger version with
      | None -> Lwt.return ""
      | Some arranger ->
        let%lwt line = M.Credit.line arranger in
        Lwt.return (spf " arranged by %s" line)
    in
    let%lwt disambiguation =
      match%lwt M.Version.disambiguation version with
      | "" -> Lwt.return ""
      | disambiguation ->
        Lwt.return (spf " (%s)" disambiguation)
    in
    spf "%d-bar %s version in %s%s%s"
      bars structure (M.Music.key_to_pretty_string key) arranger disambiguation
    |> Lwt.return

  let description_lwt version =
    Lwt.bind version description

  let name_and_disambiguation version =
    (* FIXME: put the disambiguation in a class "dim" that makes it gray-ish.
       Not sure it can really be done in formatters though? *)
    let%lwt tune = M.Version.tune version in
    let%lwt name = M.Tune.name tune in
    match%lwt M.Version.disambiguation version with
    | "" -> Lwt.return name
    | disambiguation ->
      Lwt.return (spf "%s (%s)" name disambiguation)

  let name_and_disambiguation_lwt version =
    Lwt.bind version name_and_disambiguation

  let author_and_arranger ?(short=true) version =
    (* FIXME: put the arranger in a class "dim" that makes it gray-ish? Not sure
       it can really be done in formatters though? *)
    let%lwt tune = M.Version.tune version in
    let%lwt author = M.Tune.author tune in
    let%lwt arranger = M.Version.arranger version in
    let arr = if short then "arr." else "arranged by" in
    match author, arranger with
    | None, None -> Lwt.return ""
    | Some author, None -> M.Credit.line author
    | None, Some arranger ->
      let%lwt arranger = M.Credit.line arranger in
      Lwt.return (spf "%s %s" arr arranger)
    | Some author, Some arranger ->
      let%lwt author = M.Credit.line author in
      let%lwt arranger = M.Credit.line arranger in
      Lwt.return (spf "%s, %s %s" author arr arranger)

end
