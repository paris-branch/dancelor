open Js_of_ocaml
module Html = Dom_html

type dom_node = Dom.node Js.t
type document = Dom_html.document Js.t

type node = document -> dom_node

let node_of_dom_node dom_node =
  fun _document -> dom_node

let append_nodes
    (parent : #Dom.node Js.t)
    (document : Html.document Js.t)
    (children : (Html.document Js.t -> #Dom.node Js.t) list)
  =
  children
  |> List.map ((|>) document)
  |> List.iter (Dom.appendChild parent)

let text_lwt str_lwt (document : document) =
  let text = document##createTextNode (Js.string "") in
  Lwt.on_success str_lwt (fun str ->
      text##.data := Js.string str);
  (text :> dom_node)

let text str document =
  text_lwt (Lwt.return str) document

let elt_lwt create ?(classes=[]) children_lwt document =
  let (elt : 'element Js.t) = create document in
  List.iter (fun class_ -> elt##.classList##add (Js.string class_)) classes;
  Lwt.on_success children_lwt (fun children ->
      append_nodes elt document children);
  elt

let h1_lwt ?classes children_lwt document =
  (elt_lwt Html.createH1 ?classes children_lwt document :> dom_node)
let h1 ?classes children document =
  h1_lwt ?classes (Lwt.return children) document

let h2_lwt ?classes children_lwt document =
  (elt_lwt Html.createH2 ?classes children_lwt document :> dom_node)
let h2 ?classes children document =
  h2_lwt ?classes (Lwt.return children) document

let h3_lwt ?classes children_lwt document =
  (elt_lwt Html.createH3 ?classes children_lwt document :> dom_node)
let h3 ?classes children document =
  h3_lwt ?classes (Lwt.return children) document

let h4_lwt ?classes children_lwt document =
  (elt_lwt Html.createH4 ?classes children_lwt document :> dom_node)
let h4 ?classes children document =
  h4_lwt ?classes (Lwt.return children) document

let h5_lwt ?classes children_lwt document =
  (elt_lwt Html.createH5 ?classes children_lwt document :> dom_node)
let h5 ?classes children document =
  h5_lwt ?classes (Lwt.return children) document

let h6_lwt ?classes children_lwt document =
  (elt_lwt Html.createH6 ?classes children_lwt document :> dom_node)
let h6 ?classes children document =
  h6_lwt ?classes (Lwt.return children) document

let p_lwt ?classes children_lwt document =
  (elt_lwt Html.createP ?classes children_lwt document :> dom_node)
let p ?classes children document =
  p_lwt ?classes (Lwt.return children) document

let div_lwt ?classes children_lwt document =
  (elt_lwt Html.createDiv ?classes children_lwt document :> dom_node)
let div ?classes children document =
  div_lwt ?classes (Lwt.return children) document

let a_lwt ?href ?href_lwt ?classes children_lwt document =
  let href_lwt =
    match href, href_lwt with
    | None, None | Some _, Some _ -> invalid_arg "Dancelor_client_elements.H.a"
    | Some href, None -> Lwt.return href
    | None, Some href_lwt -> href_lwt
  in
  let a = elt_lwt Html.createA ?classes children_lwt document in
  Lwt.on_success href_lwt (fun href ->
      a##.href := Js.string href);
  (a :> dom_node)

let a ?href ?href_lwt ?classes children document =
  a_lwt ?href ?href_lwt ?classes (Lwt.return children) document
