open Js_of_ocaml
open Dancelor_client_elements

module Html = Dom_html

let js = Js.string

module type PAGE = sig
  type t
  val contents : t -> Html.divElement Js.t
  val init : t -> unit
  val refresh : t -> unit
end

let pack (type s) (module M : PAGE with type t = s) (create : Page.t -> s) = 
  (module struct
    type t = M.t
    let create = create
    let contents t = M.contents t
    let init t = M.init t
    let refresh t = M.refresh t
  end : Page.CONTENTS)

let dispatch url =
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
    pack (module TuneExplorer) TuneExplorer.create
  | ["tune";slug] ->
    pack (module TuneViewer) (TuneViewer.create slug)
  | ["set";"all"] ->
    pack (module SetExplorer) SetExplorer.create
  | ["set";"compose"] ->
    pack (module ComposerInterface) ComposerInterface.create
  | ["set";slug] ->
    pack (module SetViewer) (SetViewer.create slug)
  | ["program";"all"] ->
    pack (module ProgramExplorer) ProgramExplorer.create
  | ["program";slug] ->
    pack (module ProgramViewer) (ProgramViewer.create slug)
  | ["credit";"add"] ->
    pack (module CreditEditorInterface) (fun page -> CreditEditorInterface.create page)
  | [] ->
    pack (module Index) Index.create
  | _ ->
    pack (module UnknownPage) UnknownPage.create
  end
