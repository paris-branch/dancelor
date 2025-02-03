open Html

type t = {
  parent_title: string;
  title: string S.t;
  content: Html_types.div_content_fun elt list;
}

let get_title p =
  Fun.flip S.map p.title @@ function
  | "" -> p.parent_title
  | title ->
    match p.parent_title with
    | "" -> title
    | _ -> title ^ " | " ^ p.parent_title

let get_content p = [h2 ~a: [a_class ["title"]] [R.txt p.title]] @ p.content

let make ?(parent_title = "") ~title content = {parent_title; title; content}
