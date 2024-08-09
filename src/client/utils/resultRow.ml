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

let make ?(classes = []) ?action ?href cells =
  let action = match (action, href) with
    | (None, None) -> NoAction
    | (Some a, None) -> a
    | (None, Some href) -> Link (S.const href)
    | (Some _, Some _) -> invalid_arg "Cannot have both action and href"
  in
  {action; cells; classes}

(** Generic row showing an emoji on the left and a message on the right. *)
let icon_row ?classes ?action icon message =
  let open Dancelor_client_html in
  make ?classes ?action
    [
      td ~a:[a_colspan 9999] [
        i ~a:[a_class ["material-symbols-outlined"]] [txt icon];
        txt " ";
        txt message;
      ];
    ]

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

let run_action row =
  match row.action with
  | NoAction -> ()
  | Link href -> Dom_html.window##.location##.href := Js.string (S.value href)
  | Callback f -> f ()
