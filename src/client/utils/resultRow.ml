open React
open Js_of_ocaml
open Js_of_ocaml_tyxml.Tyxml_js

type action =
  | NoAction
  | Link of string S.t
  | Callback of (unit -> unit) (* FIXME: Lwt.t? *)
[@@deriving variants]

type t = {
  action : action;
  cells : Html_types.td Html.elt list;
  classes : string list;
}

let make ?(classes = []) ?(action = NoAction) cells =
  {action; cells; classes}

(* FIXME: this is very similar to [Dancelor_client_tables.clickable_row]; those
   two should be merged in a common notion (probably that ot
   [Dancelor_client_tables]). *)
(* FIXME: When [onclick] is used as an [a], we could do better and actually have
   an [<a />] element *)

let to_clickable_row t =
  let open Dancelor_client_html in
  match t.action with
  | NoAction -> tr ~a:[a_class t.classes] t.cells
  | Link href ->
    tr ~a:[
      a_class (["clickable"] @ t.classes);
      a_onclick (fun _ -> Dom_html.window##.location##.href := Js.string (S.value href); true);
    ] t.cells
  | Callback f ->
    tr ~a:[
      a_class (["clickable"] @ t.classes);
      a_onclick (fun _ -> f (); true);
    ] t.cells
