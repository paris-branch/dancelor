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

type 'children node_maker = ?classes:string list -> 'children -> node

type std_node_maker_lwt = node list Lwt.t node_maker
type std_node_maker     = node list       node_maker

let gen_node_lwt create ?(classes=[]) children_lwt document =
  let (elt : 'element Js.t) = create document in
  List.iter (fun class_ -> elt##.classList##add (Js.string class_)) classes;
  Lwt.on_success children_lwt (fun children ->
      append_nodes elt document children);
  elt

let node_lwt create ?classes children_lwt document =
  (gen_node_lwt create ?classes children_lwt document :> dom_node)

let node create ?classes children document =
  node_lwt create ?classes (Lwt.return children) document

let h1_lwt = node_lwt Dom_html.createH1
let h1     = node     Dom_html.createH1

let h2_lwt = node_lwt Dom_html.createH2
let h2     = node     Dom_html.createH2

let h3_lwt = node_lwt Dom_html.createH3
let h3     = node     Dom_html.createH3

let h4_lwt = node_lwt Dom_html.createH4
let h4     = node     Dom_html.createH4

let h5_lwt = node_lwt Dom_html.createH5
let h5     = node     Dom_html.createH5

let h6_lwt = node_lwt Dom_html.createH6
let h6     = node     Dom_html.createH6

let p_lwt = node_lwt Dom_html.createP
let p     = node     Dom_html.createP

let div_lwt = node_lwt Dom_html.createDiv
let div     = node     Dom_html.createDiv

let span_lwt = node_lwt Dom_html.createSpan
let span     = node     Dom_html.createSpan

type target = Blank | Self | Parent | Top | Frame of string

let a_lwt ?href ?href_lwt ?target ?classes children_lwt document =
  let href_lwt =
    match href, href_lwt with
    | None, None | Some _, Some _ -> invalid_arg "Dancelor_client_html.a"
    | Some href, None -> Lwt.return href
    | None, Some href_lwt -> href_lwt
  in
  let a = gen_node_lwt Dom_html.createA ?classes children_lwt document in
  Lwt.on_success href_lwt (fun href ->
      a##.href := Js.string href);
  (match target with
   | None -> ()
   | Some target ->
     a##.target := Js.string
         (match target with
          | Blank -> "_blank"
          | Self -> "_self"
          | Parent -> "_parent"
          | Top -> "_top"
          | Frame name -> name));
  (a :> dom_node)

let a ?href ?href_lwt ?target ?classes children document =
  a_lwt ?href ?href_lwt ?target ?classes (Lwt.return children) document

let table_lwt = node_lwt Dom_html.createTable
let table     = node     Dom_html.createTable

let thead_lwt = node_lwt Dom_html.createThead
let thead     = node     Dom_html.createThead

let tbody_lwt = node_lwt Dom_html.createTbody
let tbody     = node     Dom_html.createTbody

let tfoot_lwt = node_lwt Dom_html.createTfoot
let tfoot     = node     Dom_html.createTfoot

let tr_lwt = node_lwt Dom_html.createTr
let tr     = node     Dom_html.createTr

let td_lwt ?colspan ?rowspan ?classes children_lwt document =
  let td = gen_node_lwt Dom_html.createTd ?classes children_lwt document in
  (match colspan with None -> () | Some colspan -> td##.colSpan := colspan);
  (match rowspan with None -> () | Some rowspan -> td##.rowSpan := rowspan);
  (td :> dom_node)

let td ?colspan ?rowspan ?classes children document =
  td_lwt ?colspan ?rowspan ?classes (Lwt.return children) document

let th_lwt ?colspan ?rowspan ?classes children_lwt document =
  let th = gen_node_lwt Dom_html.createTh ?classes children_lwt document in
  (match colspan with None -> () | Some colspan -> th##.colSpan := colspan);
  (match rowspan with None -> () | Some rowspan -> th##.rowSpan := rowspan);
  (th :> dom_node)

let th ?colspan ?rowspan ?classes children document =
  th_lwt ?colspan ?rowspan ?classes (Lwt.return children) document

let ol = node Dom_html.createOl
let ol_lwt = node_lwt Dom_html.createOl

let ul = node Dom_html.createUl
let ul_lwt = node_lwt Dom_html.createUl

let li = node Dom_html.createLi
let li_lwt = node_lwt Dom_html.createLi

let br document = (Dom_html.createBr document :> dom_node)
let hr document = (Dom_html.createHr document :> dom_node)

let img ?src ?src_lwt ?classes () document =
  let src_lwt =
    match src, src_lwt with
    | None, None | Some _, Some _ -> invalid_arg "Dancelor_client_html.img"
    | Some src, _ -> Lwt.return src | _, Some src_lwt -> src_lwt
  in
  let img = gen_node_lwt Dom_html.createImg ?classes Lwt.return_nil document in
  Lwt.on_success src_lwt (fun src ->
      img##.src := Js.string src);
  (img :> dom_node)

let audio ?src ?src_lwt ?(controls=false) ?classes () document =
  let src_lwt =
    match src, src_lwt with
    | None, None | Some _, Some _ -> invalid_arg "Dancelor_client_html.audio"
    | Some src, _ -> Lwt.return src | _, Some src_lwt -> src_lwt
  in
  let audio = gen_node_lwt Dom_html.createAudio ?classes Lwt.return_nil document in
  audio##.controls := Js.bool controls;
  Lwt.on_success src_lwt (fun src ->
    audio##.src := Js.string src);
  (audio :> dom_node)
