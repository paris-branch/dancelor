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

end

module Credit = struct

  let line = function
    | None -> Lwt.return ""
    | Some c -> M.Credit.line c

end

module Tune = struct

  let description tune =
    (* FIXME: make author a link *)
    let%lwt tune = tune in
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

  let aka tune =
    match%lwt Lwt.bind tune M.Tune.alt_names with
    | [] -> Lwt.return ""
    | names -> Printf.sprintf "Also known as: %s" (String.concat ", " names) |> Lwt.return

end

module Version = struct

  let description version =
    (* FIXME: make arranger a link *)
    let%lwt version = version in
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

end
