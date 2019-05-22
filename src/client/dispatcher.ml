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
    TuneExplorer.contents (TuneExplorer.create page)
  | ["tune";slug] -> 
    TuneViewer.contents (TuneViewer.create page slug)
  | [] ->
    Index.contents (Index.create ())
  | _ -> 
    UnknownPage.contents (UnknownPage.create ())
  end


