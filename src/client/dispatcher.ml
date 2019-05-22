open Js_of_ocaml
open Dancelor_client_elements

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
  | [] ->
    Index.contents (Index.create ())
  | _ -> 
    UnknownPage.contents (UnknownPage.create ())
  end


