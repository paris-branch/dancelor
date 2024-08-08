open Nes
open Js_of_ocaml

module AnyResult = AnyResult
module ResultRow = ResultRow

let rec is_child_of : 'a 'b. ((#Dom.node as 'a) Js.t) -> ((#Dom.node as 'b) Js.t) -> bool =
  fun c p ->
  ((c :> Dom.node Js.t) = (p :> Dom.node Js.t)) ||
  (Js.Opt.case c##.parentNode
     (Fun.const false)
     (fun p' -> is_child_of p' p))

let is_input : 'a. (#Dom.element as 'a) Js.t -> bool =
  fun n ->
  let tag = String.lowercase_ascii (Js.to_string n##.tagName) in
  tag = "input" || tag = "textarea"

let add_target_event_listener n ev f =
  let open Dom_html in
  ignore @@
  addEventListener n ev
    (handler @@ fun event ->
     Js.Opt.case event##.target (fun () -> Js._true) (f event))
    Js._false (* default: run in bubbling phase *)
