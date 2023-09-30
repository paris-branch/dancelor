open Nes
open Dancelor_common
open Dancelor_client_html.NewAPI
module M = Dancelor_client_model

let name ?(link=true) version =
  let name_text = [R.txt @@ S.from' "" (M.Version.tune version >>=| M.Tune.name)] in
  if link then
    let href =
      let%lwt slug = M.Version.slug version in
      Lwt.return PageRouter.(path (Version slug))
    in
    Lwt.return [a ~a:[R.a_href @@ S.from' "" href] name_text]
  else
    Lwt.return name_text

let disambiguation_and_sources version =
  let sources_lwt =
    let%lwt sources =
      let filter = M.Book.Filter.(
          M.Formula.and_ (memVersionDeep version) isSource
        )
      in
      M.Book.search filter
      >|=| M.Score.list_erase
    in
    match%lwt Lwt_list.map_p BookNewAPI.short_title sources with
    | [] -> Lwt.return_nil
    | [title] -> Lwt.return (txt "Source: " :: title)
    | titles ->
      titles
      |> List.intertwine (fun _ -> [txt " - "])
      |> List.flatten
      |> List.cons (txt "Sources: ")
      |> Lwt.return
  in
  Lwt.return [
    R.txt @@ S.from' "" (M.Version.disambiguation version);
    R.span ~a:[a_class ["dim"; "details"]] @@ RList.from_lwt' [] sources_lwt
  ]
