open Js_of_ocaml_tyxml.Tyxml_js

type t = {
  title: string React.S.t;
  content: Html_types.div Html.elt;
}

let get_title p = p.title

let get_content p = p.content

let make ~title content = {title; content}

let sub_title category title =
  Fun.flip React.S.map title @@ function
  | "" -> category
  | title -> title ^ " | " ^ category
