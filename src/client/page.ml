open Js_of_ocaml_tyxml.Tyxml_js

type t = {
  title: string React.S.t;
  content: Html_types.div Html.elt;
}

let get_title p = p.title

let get_content p = p.content

let make ?(parent_title = "") ~title content =
  let title =
    Fun.flip React.S.map title @@ function
    | "" -> parent_title
    | title ->
      match parent_title with
      | "" -> title
      | _ -> title ^ " | " ^ parent_title
  in
  {title; content}
