open Js_of_ocaml

module Html = Dom_html

let js = Js.string

let get_contents page =
  let url =
    Html.window##.location##.href
    |> Js.to_string
    |> Uri.of_string
  in
  let path =
    Uri.path url
    |> String.split_on_char '/'
  in
  let rec trim = function
    | "" :: l -> trim l
    | l -> l
  in
  begin match trim path with
  | ["tune";"all"] ->
    (fun () -> ()),
    TuneExplorer.contents (TuneExplorer.create page)
  | ["tune";slug] ->
    (fun () -> ()),
    TuneViewer.contents (TuneViewer.create page slug)
  | ["set";"all"] ->
    (fun () -> ()),
    SetExplorer.contents (SetExplorer.create page)
  | ["set";"compose"] ->
    let interface = ComposerInterface.create page in
    (fun () -> ComposerInterface.refresh interface),
    ComposerInterface.contents interface
  | ["set";slug] ->
    (fun () -> ()),
    SetViewer.contents (SetViewer.create page slug)
  | ["program";"all"] ->
    (fun () -> ()),
    ProgramExplorer.contents (ProgramExplorer.create page)
  | ["program";slug] ->
    (fun () -> ()),
    ProgramViewer.contents (ProgramViewer.create page slug)
  | ["credit";"add"] ->
    let interface = CreditEditorInterface.create page in
    (fun () -> CreditEditorInterface.refresh interface),
    CreditEditorInterface.contents interface
  | [] ->
    (fun () -> ()),
    Index.contents (Index.create page)
  | _ ->
    (fun () -> ()),
    UnknownPage.contents (UnknownPage.create ())
  end
