open Js_of_ocaml

module Html = Dom_html

let js = Js.string

let add_children t cs =
  List.iter (Dom.appendChild t) cs

let extract_children t =
  let rec extract_children acc =
    if Js.to_bool t##hasChildNodes then
      (
        let child = Js.Opt.get t##.firstChild (fun () -> assert false) in
        t##removeChild child |> ignore;
        extract_children (child :: acc)
      )
    else
      List.rev acc
  in
  extract_children []

let clear_children t = ignore (extract_children t)

let rec is_child_of : 'a 'b.((#Dom.node as 'a ) Js.t) -> ((#Dom.node as 'b ) Js.t) -> bool = fun c p ->
    Printf.printf "Checking %s to %s\n" (Js.to_string c##.nodeName) (Js.to_string p##.nodeName);
    ((c :> Dom.node Js.t) = (p :> Dom.node Js.t))
    || (
      Js.Opt.case
        c##.parentNode
        (fun () -> print_endline "No parent"; false)
        (fun p' -> is_child_of p' p)
    )
