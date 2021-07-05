open Js_of_ocaml

type dom_node = Dom.node Js.t
type document = Dom_html.document Js.t

type node = document -> dom_node

let node_of_dom_node dom_node =
  fun _document -> dom_node (* FIXME: assert that document is the same *)

let node_to_dom_node document node =
  node document

let nodes_to_dom_nodes document nodes =
  List.map (node_to_dom_node document) nodes

let append_node (parent : #Dom.node Js.t) (document : document) (node : node) =
  Dom.appendChild parent (node_to_dom_node document node)

let append_nodes (parent : #Dom.node Js.t) (document : document) (nodes : node list) =
  List.iter (append_node parent document) nodes

let text_lwt str_lwt (document : document) =
  let text = document##createTextNode (Js.string "") in
  Lwt.on_success str_lwt (fun str ->
      text##.data := Js.string str);
  (text :> dom_node)

let text str document =
  text_lwt (Lwt.return str) document

type node_maker_lwt = ?classes:string list -> node list Lwt.t -> node
type node_maker = ?classes:string list -> node list -> node

let elt_lwt create ?(classes=[]) children_lwt document =
  let (elt : 'element Js.t) = create document in
  List.iter (fun class_ -> elt##.classList##add (Js.string class_)) classes;
  Lwt.on_success children_lwt (fun children ->
      append_nodes elt document children);
  elt

let h1_lwt ?classes children_lwt document =
  (elt_lwt Dom_html.createH1 ?classes children_lwt document :> dom_node)
let h1 ?classes children document =
  h1_lwt ?classes (Lwt.return children) document

let h2_lwt ?classes children_lwt document =
  (elt_lwt Dom_html.createH2 ?classes children_lwt document :> dom_node)
let h2 ?classes children document =
  h2_lwt ?classes (Lwt.return children) document

let h3_lwt ?classes children_lwt document =
  (elt_lwt Dom_html.createH3 ?classes children_lwt document :> dom_node)
let h3 ?classes children document =
  h3_lwt ?classes (Lwt.return children) document

let h4_lwt ?classes children_lwt document =
  (elt_lwt Dom_html.createH4 ?classes children_lwt document :> dom_node)
let h4 ?classes children document =
  h4_lwt ?classes (Lwt.return children) document

let h5_lwt ?classes children_lwt document =
  (elt_lwt Dom_html.createH5 ?classes children_lwt document :> dom_node)
let h5 ?classes children document =
  h5_lwt ?classes (Lwt.return children) document

let h6_lwt ?classes children_lwt document =
  (elt_lwt Dom_html.createH6 ?classes children_lwt document :> dom_node)
let h6 ?classes children document =
  h6_lwt ?classes (Lwt.return children) document

let p_lwt ?classes children_lwt document =
  (elt_lwt Dom_html.createP ?classes children_lwt document :> dom_node)
let p ?classes children document =
  p_lwt ?classes (Lwt.return children) document

let div_lwt ?classes children_lwt document =
  (elt_lwt Dom_html.createDiv ?classes children_lwt document :> dom_node)
let div ?classes children document =
  div_lwt ?classes (Lwt.return children) document

let span_lwt ?classes children_lwt document =
  (elt_lwt Dom_html.createSpan ?classes children_lwt document :> dom_node)
let span ?classes children document =
  span_lwt ?classes (Lwt.return children) document

let a_lwt ?href ?href_lwt ?classes children_lwt document =
  let href_lwt =
    match href, href_lwt with
    | None, None | Some _, Some _ -> invalid_arg "Dancelor_client_html.a"
    | Some href, None -> Lwt.return href
    | None, Some href_lwt -> href_lwt
  in
  let a = elt_lwt Dom_html.createA ?classes children_lwt document in
  Lwt.on_success href_lwt (fun href ->
      a##.href := Js.string href);
  (a :> dom_node)

let a ?href ?href_lwt ?classes children document =
  a_lwt ?href ?href_lwt ?classes (Lwt.return children) document

let br document = (Dom_html.createBr document :> dom_node)
let hr document = (Dom_html.createHr document :> dom_node)

let img ?src ?src_lwt ?classes () document =
  let src_lwt =
    match src, src_lwt with
    | None, None | Some _, Some _ -> invalid_arg "Dancelor_client_html.img"
    | Some src, None -> Lwt.return src
    | None, Some src_lwt -> src_lwt
  in
  let img = elt_lwt Dom_html.createImg ?classes Lwt.return_nil document in
  Lwt.on_success src_lwt (fun src ->
      img##.src := Js.string src);
  (img :> dom_node)
