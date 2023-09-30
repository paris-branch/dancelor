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
      until the [promise] resolves, and changes every time [promise] resolves
      again after that. *)
  let from (placeholder: 'a) (promise : unit -> 'a Lwt.t) : 'a Lwt_react.signal =
    let result, send_result = create placeholder in
    let rec loop () =
      Lwt.bind (promise ()) @@ fun result ->
      send_result result;
      loop ()
    in
    Lwt.async loop;
    result

  (** [from' ~placeholder promise] creates a signal that holds the [placeholder]
      until the [promise] resolves. This is similar to {!from} except it does
      only one update. *)
  let from' (placeholder: 'a) (promise : 'a Lwt.t) : 'a Lwt_react.signal =
    let result, send_result = create placeholder in
    Lwt.on_success promise send_result;
    result
end

(** Reactive lists. *)
module RList = struct
  include ReactiveData.RList

  (** [from_lwt placeholder promise] is a container whose initial value is
      [placeholder], and which gets updated every time [promise] resolves. When
      that happens we detect the differences between the previous value and
      [promise]'s result, and perform downstream computation (e.g., for [map])
      only on the new and modified elements. *)
  let from_lwt placeholder promise =
    from_signal @@ S.from placeholder promise

  (** [from_lwt' placeholder promise] is a container whose initial value is
      [placeholder], and which gets updated once [promise] resolves. When that
      happens we detect the differences between [placeholder] and [promise]'s
      result, and perform downstream computation (e.g., for [map]) only on the
      new and modified elements. *)
  let from_lwt' placeholder promise =
    from_signal @@ S.from' placeholder promise
end
