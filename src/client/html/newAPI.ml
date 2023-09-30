(** {1 HTML} *)

(** {2 TyXML aliases} *)

module C = Js_of_ocaml_tyxml.Tyxml_js.Html
include C
(** Constant HTML nodes. *)

module R = Js_of_ocaml_tyxml.Tyxml_js.R.Html
(** Reactive HTML nodes. *)

(* FIXME: add a module for “Lwt” HTML nodes. In the meantime, one can simply use
   [R] and the function [S.from'] below. *)

module To_dom = Js_of_ocaml_tyxml.Tyxml_js.To_dom
(** Conversion from TyXML nodes to Dom ones. *)

(** {2 React aliases} *)

(** Reactive signals. *)
module S = struct
  include Lwt_react.S

  (** [from' ~placeholder promise] creates a signal that holds the [placeholder]
      until the [promise] resolves. *)
  let from' ~(placeholder: 'a) (promise : 'a Lwt.t) : 'a Lwt_react.signal =
    let result, send_result = create placeholder in
    Lwt.on_success promise send_result;
    result
end

(** Reactive lists. *)
module RList = ReactiveData.RList
