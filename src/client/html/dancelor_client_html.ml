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

let append_nodes (parent : #Dom.node Js.t) (document : document) (nodes : node list) =
  List.iter (Dom.appendChild parent % node_to_dom_node document) nodes

let set_nodes (parent : #Dom.node Js.t) (document : document) (nodes : node list) =
  List.iter (Dom.removeChild parent) (Dom.list_of_nodeList parent##.childNodes);
  List.iter (Dom.appendChild parent % node_to_dom_node document) nodes

type 'a provider =
  | Const of 'a
  | Lwt of 'a Lwt.t
  | Loop of (unit -> 'a Lwt.t)

type ('input, 'output) kind = 'input -> 'output provider

let const x = Const x
let lwt x = Lwt x
let loop x = Loop x

let on_provided f = function
  | Const y -> f y
  | Lwt y -> Lwt.on_success y f
  | Loop y ->
    let rec loop () = Lwt.bind (y ()) (fun y -> f y; loop ()) in
    Lwt.async loop

let on_provided_kind k x f = on_provided f (k x)
let on_provided_option x f = Option.ifsome (on_provided f) x

let text kind data (document : document) =
  let text = document##createTextNode (Js.string "") in
  on_provided_kind kind data (fun data -> text##.data := Js.string data);
  (text :> dom_node)

let make_node create ?(classes=[]) kind children document =
  let (element : 'element Js.t) = create document in
  List.iter (fun class_ -> element##.classList##add (Js.string class_)) classes;
  on_provided_kind kind children (set_nodes element document);
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

let a ?href ?target ?classes kind children document =
  let a = make_node Dom_html.createA ?classes kind children document in
  on_provided_option href (fun href -> a##.href := Js.string href);
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

type type_ =
  | Checkbox
  | Text

let input ~type_ ?classes ?placeholder ?on_input () document =
  let type_ = match type_ with
    | Checkbox -> "checkbox"
    | Text -> "text"
  in
  let input =
    make_node
      (Dom_html.createInput ~_type:(Js.string type_))
      ?classes
      const
      []
      document
  in
  Option.ifsome (fun t -> input##.placeholder := Js.string t) placeholder;
  Option.ifsome
    (fun on_input ->
       input##.oninput := Dom.handler @@ fun _ ->
         on_input (Js.to_string input##.value);
         Js.bool false)
    on_input;
  (input :> dom_node)

let br document = (Dom_html.createBr document :> dom_node)
let hr document = (Dom_html.createHr document :> dom_node)

let img ?src ?classes () document =
  let img = make_node Dom_html.createImg ?classes const [] document in
  on_provided_option src (fun src -> img##.src := Js.string src);
  (img :> dom_node)

let object_ ~type_ ?data ?classes kind children document =
  let object_ = make_node Dom_html.createObject ?classes kind children document in
  object_##._type := Js.string type_;
  on_provided_option data (fun data -> object_##.data := Js.string data);
  (object_ :> dom_node)

let audio ?src ?(controls=false) ?classes () document =
  let audio = make_node Dom_html.createAudio ?classes const [] document in
  audio##.controls := Js.bool controls;
  on_provided_option src (fun src -> audio##.src := Js.string src);
  (audio :> dom_node)
