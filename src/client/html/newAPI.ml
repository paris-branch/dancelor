(** {1 HTML} *)

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

(** {2 TyXML aliases} *)

module C = Js_of_ocaml_tyxml.Tyxml_js.Html
include C
(** Constant HTML nodes. *)

(** Lwt HTML nodes. *)
module L = struct
  (* NOTE: The following relies on [Tyxml_js]'s React-based HTML builders but
     wraps them to make Lwt-based builders. The cleaner version would be to rely
     on [Tyxml]'s functorial interface, but we could not manage to use it while
     also keeping the right type equalities with other HTML builder modules. *)

  module R = Js_of_ocaml_tyxml.Tyxml_js.R.Html

  (* NOTE: To be filled on demand. *)

  let txt str = R.txt (S.from' "" str)

  let a_href uri = R.a_href (S.from' "" uri)

  let div ?a elts = R.div ?a (RList.from_lwt' [] elts)
  let h1 ?a elts = R.h1 ?a (RList.from_lwt' [] elts)
  let h2 ?a elts = R.h2 ?a (RList.from_lwt' [] elts)
  let h3 ?a elts = R.h3 ?a (RList.from_lwt' [] elts)
  let h4 ?a elts = R.h4 ?a (RList.from_lwt' [] elts)
  let h5 ?a elts = R.h5 ?a (RList.from_lwt' [] elts)
  let h6 ?a elts = R.h6 ?a (RList.from_lwt' [] elts)
  let span ?a elts = R.span ?a (RList.from_lwt' [] elts)
  let tbody ?a elts = R.tbody ?a (RList.from_lwt' [] elts)
  let td ?a elts = R.td ?a (RList.from_lwt' [] elts)
end

(* FIXME: Add an [LL] module for Lwt-loop-based builders. *)

module To_dom = Js_of_ocaml_tyxml.Tyxml_js.To_dom
(** Conversion from TyXML nodes to Dom ones. *)
