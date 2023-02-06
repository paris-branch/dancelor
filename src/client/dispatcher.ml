open Nes
open Js_of_ocaml
open Dancelor_common
open Dancelor_client_elements
open Dancelor_client_views

module Html = Dom_html

let js = Js.string

module type PAGE = sig
  type t
  val contents : t -> Html.divElement Js.t
  val init : t -> unit
  val refresh : t -> unit
end

let pack(type s) (module M: PAGE with type t = s) (create : Page.t -> s) =
  (module struct
    type t = M.t
    let create = create
    let contents t = M.contents t
    let init t = M.init t
    let refresh t = M.refresh t
  end: Page.CONTENTS)

let dispatch url =
  let path =
    let rec trim = function
      | "" :: l -> trim l
      | l -> l
    in
    Uri.path url |> String.split_on_char '/' |> trim
  in
  let query_parameters = QueryParameters.from_uri url in
  match path with
  | ["search"] ->
    let q = QueryParameters.get_string "q" query_parameters in
    pack (module Search) (Search.create q)
  | ["version"; "add"] ->
    pack (module VersionEditorInterface) VersionEditorInterface.create
  | ["version"; "all"] ->
    pack (module VersionExplorer) VersionExplorer.create
  | ["version"; "broken"] ->
    pack (module VersionBrokenExplorer) VersionBrokenExplorer.create
  | ["version"; slug] ->
    pack (module VersionViewer) (VersionViewer.create (Slug.unsafe_of_string slug))
  | ["tune"; slug] ->
    pack (module TuneViewer) (TuneViewer.create (Slug.unsafe_of_string slug))
  | ["set"; "all"] ->
    pack (module SetExplorer) SetExplorer.create
  | ["set"; "compose"] ->
    pack (module SetEditorInterface) SetEditorInterface.create
  | ["set"; slug] ->
    pack (module SetViewer) (SetViewer.create (Slug.unsafe_of_string slug))
  | ["book"; "all"] ->
    pack (module BookExplorer) BookExplorer.create
  | ["book"; "compose"] ->
    pack (module BookEditorInterface) BookEditorInterface.create
  | ["book"; slug] ->
    pack (module BookViewer) (BookViewer.create (Slug.unsafe_of_string slug))
  | ["credit"; "add"] ->
    pack (module CreditEditorInterface) (fun page -> CreditEditorInterface.create page)
  | ["credit"; slug] ->
    pack (module CreditViewer) (CreditViewer.create (Slug.unsafe_of_string slug))
  | ["person"; slug] ->
    pack (module PersonViewer) (PersonViewer.create (Slug.unsafe_of_string slug))
  | ["dance"; slug] ->
    pack (module DanceViewer) (DanceViewer.create (Slug.unsafe_of_string slug))
  | [] ->
    pack (module Index) Index.create
  | _ ->
    pack (module UnknownPage) UnknownPage.create
