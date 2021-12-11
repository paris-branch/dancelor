open Nes
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
  | ["search"] ->
    (match List.assoc_opt "q" (Uri.query url) with
     | Some [q] ->
       (match Yojson.Safe.from_string q with
        | `String q -> pack (module Search) (Search.create (Some q))
        | _ -> assert false)
     | _ -> pack (module Search) (Search.create None))
  | ["version"; "all"] ->
    pack (module VersionExplorer) VersionExplorer.create
  | ["version"; slug] ->
    pack (module VersionViewer) (VersionViewer.create (Slug.unsafe_of_string slug))
  | ["tune"; slug] ->
    pack (module TuneViewer) (TuneViewer.create (Slug.unsafe_of_string slug))
  | ["set"; "all"] ->
    pack (module SetExplorer) SetExplorer.create
  | ["set"; "compose"] ->
    pack (module ComposerInterface) ComposerInterface.create
  | ["set"; slug] ->
    pack (module SetViewer) (SetViewer.create (Slug.unsafe_of_string slug))
  | ["book"; "all"] ->
    pack (module BookExplorer) BookExplorer.create
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
  end
