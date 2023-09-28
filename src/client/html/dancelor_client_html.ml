open Nes
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

type 'a provider =
  | Const of 'a
  | Lwt of 'a Lwt.t

type ('input, 'output) kind = 'input -> 'output provider

let const x = Const x
let lwt x = Lwt x

let text kind data (document : document) =
  let text = document##createTextNode (Js.string "") in
  (
    match kind data with
    | Const data ->  text##.data := Js.string data
    | Lwt data -> Lwt.on_success data (fun data -> text##.data := Js.string data)
  );
  (text :> dom_node)

let make_node create ?(classes=[]) kind children document =
  let (element : 'element Js.t) = create document in
  List.iter (fun class_ -> element##.classList##add (Js.string class_)) classes;
  (
    match kind children with
    | Const children -> append_nodes element document children
    | Lwt children -> Lwt.on_success children (append_nodes element document)
  );
  element

let node create ?classes provide children document =
  (make_node create ?classes provide children document :> dom_node)

let h1 ?classes kind children = node Dom_html.createH1 ?classes kind children
let h2 ?classes kind children = node Dom_html.createH2 ?classes kind children
let h3 ?classes kind children = node Dom_html.createH3 ?classes kind children
let h4 ?classes kind children = node Dom_html.createH4 ?classes kind children
let h5 ?classes kind children = node Dom_html.createH5 ?classes kind children
let h6 ?classes kind children = node Dom_html.createH6 ?classes kind children

let div ?classes kind children = node Dom_html.createDiv ?classes kind children
let span ?classes kind children = node Dom_html.createSpan ?classes kind children

let p ?classes kind children = node Dom_html.createP ?classes kind children
let i ?classes kind children = node Dom_html.createI ?classes kind children

type target = Blank | Self | Parent | Top | Frame of string

let a ?href ?href_lwt ?target ?classes kind children document =
  let href_lwt =
    match href, href_lwt with
    | None, None | Some _, Some _ -> invalid_arg "Dancelor_client_html.a"
    | Some href, None -> Lwt.return href
    | None, Some href_lwt -> href_lwt
  in
  let a = make_node Dom_html.createA ?classes kind children document in
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

let table ?classes kind children = node Dom_html.createTable ?classes kind children
let thead ?classes kind children = node Dom_html.createThead ?classes kind children
let tbody ?classes kind children = node Dom_html.createTbody ?classes kind children
let tfoot ?classes kind children = node Dom_html.createTfoot ?classes kind children
let tr ?classes kind children = node Dom_html.createTr ?classes kind children

let td ?colspan ?rowspan ?classes kind children document =
  let td = make_node Dom_html.createTd ?classes kind children document in
  (match colspan with None -> () | Some colspan -> td##.colSpan := colspan);
  (match rowspan with None -> () | Some rowspan -> td##.rowSpan := rowspan);
  (td :> dom_node)

let th ?colspan ?rowspan ?classes kind children document =
  let th = make_node Dom_html.createTh ?classes kind children document in
  (match colspan with None -> () | Some colspan -> th##.colSpan := colspan);
  (match rowspan with None -> () | Some rowspan -> th##.rowSpan := rowspan);
  (th :> dom_node)

let ol ?classes kind children = node Dom_html.createOl ?classes kind children
let ul ?classes kind children = node Dom_html.createUl ?classes kind children
let li ?classes kind children = node Dom_html.createLi ?classes kind children

let label ?classes kind children = node Dom_html.createLabel ?classes kind children

type type_ = Checkbox

let input ~type_ ?classes () document =
  let type_ = match type_ with
    | Checkbox -> "checkbox"
  in
  let input =
    make_node
      (Dom_html.createInput ~_type:(Js.string type_))
      ?classes
      const
      []
      document
  in
  (input :> dom_node)

let br document = (Dom_html.createBr document :> dom_node)
let hr document = (Dom_html.createHr document :> dom_node)

let img ?src ?src_lwt ?classes () document =
  let src_lwt =
    match src, src_lwt with
    | None, None | Some _, Some _ -> invalid_arg "Dancelor_client_html.img"
    | Some src, _ -> Lwt.return src | _, Some src_lwt -> src_lwt
  in
  let img = make_node Dom_html.createImg ?classes const [] document in
  Lwt.on_success src_lwt (fun src ->
      img##.src := Js.string src);
  (img :> dom_node)

let object_ ~type_ ?data ?data_lwt ?classes kind children document =
  let data_lwt =
    match data, data_lwt with
    | None, None | Some _, Some _ -> invalid_arg "Dancelor_client_html.object_"
    | Some data, _ -> Lwt.return data | _, Some data_lwt -> data_lwt
  in
  let object_ = make_node Dom_html.createObject ?classes kind children document in
  object_##._type := Js.string type_;
  Lwt.on_success data_lwt (fun data ->
      object_##.data := Js.string data);
  (object_ :> dom_node)

let audio ?src ?src_lwt ?(controls=false) ?classes () document =
  let src_lwt =
    match src, src_lwt with
    | None, None | Some _, Some _ -> invalid_arg "Dancelor_client_html.audio"
    | Some src, _ -> Lwt.return src | _, Some src_lwt -> src_lwt
  in
  let audio = make_node Dom_html.createAudio ?classes const [] document in
  audio##.controls := Js.bool controls;
  Lwt.on_success src_lwt (fun src ->
      audio##.src := Js.string src);
  (audio :> dom_node)
