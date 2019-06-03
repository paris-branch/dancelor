open Js_of_ocaml

module Html = Dom_html

let js = Js.string

let clear_children t =
  while Js.to_bool t##hasChildNodes do
    let child = Js.Opt.get t##.firstChild (fun () -> assert false) in
    t##removeChild child |> ignore
  done

let rec is_child_of : 'a 'b. ((#Dom.node as 'a) Js.t) -> ((#Dom.node as 'b) Js.t) -> bool =
  fun c p ->
  Printf.printf "Checking %s to %s\n" (Js.to_string c##.nodeName) (Js.to_string p##.nodeName);
  ((c :> Dom.node Js.t) = (p :> Dom.node Js.t)) ||
  (Js.Opt.case c##.parentNode
    (fun () -> print_endline "No parent"; false)
    (fun p' -> is_child_of p' p))
