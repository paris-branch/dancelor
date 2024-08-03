open Js_of_ocaml_tyxml.Tyxml_js

(* FIXME: Introduce an ADT for Actions allowing to specify that some are just a
   link to another page. *)

type t = {
  action : unit -> unit; (* FIXME: Lwt.t? *)
  cells : Html_types.td Html.elt list;
  classes : string list;
}

let make ?(classes = []) ?(action = fun () -> ()) cells =
  {action; cells; classes}

(* FIXME: this is very similar to [Dancelor_client_tables.clickable_row]; those
   two should be merged in a common notion (probably that ot
   [Dancelor_client_tables]). *)
(* FIXME: When [onclick] is used as an [a], we could do better and actually have
   an [<a />] element *)

let to_clickable_row t =
  let open Dancelor_client_html in
  tr
    ~a:[
      a_class (["clickable"] @ t.classes);
      a_onclick (fun _ -> t.action (); true);
    ]
    t.cells
