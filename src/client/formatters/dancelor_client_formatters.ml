open Nes
open Js_of_ocaml
open Dancelor_client_html

module M = Dancelor_client_model
module Router = Dancelor_common.Router

let js = Js.string

module Kind = struct

  let full_string version tune =
    let open Lwt in
    let%lwt base = M.Tune.kind tune >|= M.Kind.base_to_char in
    let%lwt bars = M.Version.bars version in
    Lwt.return [text (spf "%i %c" bars base)]

end

module Credit = struct

  let line ?(link=true) credit =
    match credit with
    | None -> Lwt.return_nil
    | Some credit ->
      let line_text = [text_lwt (M.Credit.line credit)] in
      if link then
        let href_lwt =
          let%lwt slug = M.Credit.slug credit in
          Lwt.return (Router.path_of_controller (Router.Credit slug) |> snd)
        in
        Lwt.return [a ~href_lwt line_text]
      else
        Lwt.return line_text

end

module Dance = struct

  let name ?(link=true) dance =
    let name_lwt = M.Dance.name dance in
    if link then
      let href_lwt =
        let%lwt slug = M.Dance.slug dance in
        Lwt.return (Router.path_of_controller (Router.Dance slug) |> snd)
      in
      Lwt.return [ a ~href_lwt [ text_lwt name_lwt ] ]
    else
      Lwt.return [ text_lwt name_lwt ]

end

module Tune = struct

  let name ?(link=true) tune =
    let name_text = [text_lwt (M.Tune.name tune)] in
    if link then
      let href_lwt =
        let%lwt slug = M.Tune.slug tune in
        Lwt.return (Router.path_of_controller (Router.Tune slug) |> snd)
      in
      Lwt.return [a ~href_lwt name_text]
    else
      Lwt.return name_text

  let description tune =
    let%lwt kind = M.Tune.kind tune in
    let kind = M.Kind.base_to_pretty_string kind in
    let%lwt author = M.Tune.author tune in
    match author with
    | None ->
      Lwt.return [
        text (String.capitalize_ascii kind)
      ]
    | Some author when M.Credit.is_trad author ->
      Lwt.return [
        text ("Traditional " ^ kind)
      ]
    | Some author ->
      let%lwt line_block = Credit.line ~link:true (Some author) in
      Lwt.return (
        [text (String.capitalize_ascii kind ^ " by ")]
        @ line_block
      )

  let aka tune =
    match%lwt M.Tune.alternative_names tune with
    | [] -> Lwt.return_nil
    | names ->
      Lwt.return [
        text (spf "Also known as %s" (String.concat ", " names))
      ]

  let recommended tune =
    match%lwt M.Tune.dances tune with
    | [] -> Lwt.return_nil
    | dances ->
      let%lwt dance_names = Lwt_list.map_p M.Dance.name dances in
      Lwt.return [
        text (spf "Recommended for %s" (String.concat ", " dance_names))
      ]

end

module Book = struct

  let title_and_subtitle ?(link=true) book =
    let title_text = [text_lwt (M.Book.title book)] in
    if link then
      let%lwt subtitle_block =
        match%lwt M.Book.subtitle book with
        | "" -> Lwt.return_nil
        | subtitle -> Lwt.return [
            span ~classes:["details"] [text subtitle]
          ]
      in
      Lwt.return (title_text @ subtitle_block)
    else
      Lwt.return title_text

  let short_title ?(link=true) book =
    let short_title_text = [text_lwt (M.Book.short_title book)] in
    if link then
      let href_lwt =
        let%lwt slug = M.Book.slug book in
        Lwt.return (Router.path_of_controller (Router.Book slug) |> snd)
      in
      Lwt.return [a ~href_lwt short_title_text]
    else
      Lwt.return short_title_text

end

module Version = struct

  let description ?link version =
    let%lwt bars = M.Version.bars version in
    let%lwt structure = M.Version.structure version in
    let%lwt key = M.Version.key version in
    let shape = spf "%d-bar %s version in %s" bars structure (M.Music.key_to_pretty_string key) in
    let%lwt arranger_block =
      match%lwt M.Version.arranger version with
      | None -> Lwt.return_nil
      | Some arranger ->
        let%lwt line_block = Credit.line ?link (Some arranger) in
        Lwt.return ([text " arranged by "] @ line_block)
    in
    let%lwt disambiguation_block =
      match%lwt M.Version.disambiguation version with
      | "" -> Lwt.return_nil
      | disambiguation ->
        Lwt.return [text (spf " (%s)" disambiguation)]
    in
    Lwt.return ([text shape] @ arranger_block @ disambiguation_block)

  let name ?(link=true) version =
    let name_text = [text_lwt (M.Version.tune version >>=| M.Tune.name)] in
    if link then
      let href_lwt =
        let%lwt slug = M.Version.slug version in
        Lwt.return (Router.path_of_controller (Router.Version slug) |> snd)
      in
      Lwt.return [a ~href_lwt name_text]
    else
      Lwt.return name_text

  let name_and_dance ?link ?dance_link version parameters =
    let%lwt name = name ?link version in
    let%lwt dance =
      match%lwt M.VersionParameters.for_dance parameters with
      | None -> Lwt.return_nil
      | Some dance -> Lwt.return [
          span ~classes:["dim"; "details"] [
            text "For dance: "; span_lwt (Dance.name ?link:dance_link dance)
          ]]
    in
    Lwt.return (name @ dance)

  let name_and_disambiguation ?link version =
    let%lwt name_block = name ?link version in
    let%lwt disambiguation_block =
      match%lwt M.Version.disambiguation version with
      | "" -> Lwt.return_nil
      | disambiguation -> Lwt.return [
          span ~classes:["dim"] [text (spf " (%s)" disambiguation)]
        ]
    in
    Lwt.return (name_block @ disambiguation_block)

  let name_disambiguation_and_sources ?link version =
    let sources_lwt =
      let%lwt sources =
        let filter = M.BookFilter.(
            M.Formula.and_ (memVersionDeep version) isSource
          )
        in
        M.Book.search filter
        >|=| M.Score.list_erase
      in
      match%lwt Lwt_list.map_p Book.short_title sources with
      | [] -> Lwt.return_nil
      | [title] -> Lwt.return (text "Source: " :: title)
      | titles ->
        titles
        |> List.intertwine (fun _ -> [text " - "])
        |> List.flatten
        |> List.cons (text "Sources: ")
        |> Lwt.return
    in
    let%lwt name_and_disambiguation = name_and_disambiguation ?link version in
    Lwt.return (
      name_and_disambiguation
      @ [span_lwt ~classes:["dim"; "details"] sources_lwt]
    )

  let disambiguation_and_sources version =
    let sources_lwt =
      let%lwt sources =
        let filter = M.BookFilter.(
            M.Formula.and_ (memVersionDeep version) isSource
          )
        in
        M.Book.search filter
        >|=| M.Score.list_erase
      in
      match%lwt Lwt_list.map_p Book.short_title sources with
      | [] -> Lwt.return_nil
      | [title] -> Lwt.return (text "Source: " :: title)
      | titles ->
        titles
        |> List.intertwine (fun _ -> [text " - "])
        |> List.flatten
        |> List.cons (text "Sources: ")
        |> Lwt.return
    in
    Lwt.return [
      text_lwt (M.Version.disambiguation version);
      span_lwt ~classes:["dim"; "details"] sources_lwt
    ]

  let author_and_arranger ?(short=true) ?link version =
    let%lwt author_block =
      let%lwt tune = M.Version.tune version in
      match%lwt M.Tune.author tune with
      | None -> Lwt.return_nil
      | Some author -> Credit.line ?link (Some author)
    in
    let has_author =
      let%lwt tune = M.Version.tune version in
      match%lwt M.Tune.author tune with
      | None -> Lwt.return_false
      | Some _ -> Lwt.return_true
    in
    let%lwt arranger_block =
      match%lwt M.Version.arranger version with
      | None -> Lwt.return_nil
      | Some arranger ->
        let%lwt comma = if%lwt has_author then Lwt.return ", " else Lwt.return "" in
        let arr = if short then "arr." else "arranged by" in
        let%lwt arranger_block = Credit.line ?link (Some arranger) in
        Lwt.return [
          span ~classes:["dim"] (text (spf "%s%s " comma arr) :: arranger_block)
        ]
    in
    Lwt.return (author_block @ arranger_block)

end

module Set = struct

  let works set =
    match%lwt M.Set.dances set with
    | [] -> Lwt.return_nil
    | dances ->
      let%lwt dance_names = Lwt_list.map_p M.Dance.name dances in
      Lwt.return [
        text (spf "Works for %s" (String.concat ", " dance_names))
      ]

  let name ?(link=true) set =
    let name_text = [text_lwt (M.Set.name set)] in
    let%lwt is_inline = M.Set.is_slug_none set in
    if link && not is_inline then
      let href_lwt =
        let%lwt slug = M.Set.slug set in
        Lwt.return (Router.path_of_controller (Router.Set slug) |> snd)
      in
      Lwt.return [a ~href_lwt name_text]
    else
      Lwt.return name_text

  let name_and_tunes ?link ?tunes_link set =
    let%lwt name = name ?link set in
    let%lwt versions =
      let%lwt versions_and_parameters = M.Set.versions_and_parameters set in
      let%lwt versions =
        Lwt_list.map_p
          (fun (version, _) -> Version.name ?link:tunes_link version)
          versions_and_parameters
      in
      versions
      |> List.intertwine (fun _ -> [text " - "])
      |> List.flatten
      |> List.cons (text "Tunes: ")
      |> span ~classes:["dim"; "details"]
      |> List.singleton
      |> Lwt.return
    in
    Lwt.return (name @ versions)

  let name_tunes_and_dance ?link ?tunes_link ?dance_link set parameters =
    let%lwt name_and_tunes = name_and_tunes ?link ?tunes_link set in
    let%lwt dance =
      match%lwt M.SetParameters.for_dance parameters with
      | None -> Lwt.return_nil
      | Some dance -> Lwt.return [
          span ~classes:["dim"; "details"] [
            text "For dance: "; span_lwt (Dance.name ?link:dance_link dance)
          ]]
    in
    Lwt.return (name_and_tunes @ dance)

end
