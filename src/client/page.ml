open Html

type t = {
  parent_title: string;
  before_title: Html_types.div_content_fun elt list;
  title: string S.t;
  content: Html_types.div_content_fun elt list;
}

let full_title p =
  Fun.flip S.map p.title @@ function
  | "" -> p.parent_title
  | title ->
    match p.parent_title with
    | "" -> title
    | _ -> title ^ " | " ^ p.parent_title

let content p =
  p.before_title @
  [h2 ~a: [a_class ["title"]] [R.txt p.title]] @ p.content

let make ?(parent_title = "") ~title ?(before_title = []) content = {parent_title; before_title; title; content}
