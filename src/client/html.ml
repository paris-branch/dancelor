(** {1 HTML} *)

open Nes
open ReactiveData

(** {2 React aliases} *)

(** Reactive signals. *)
module S = struct
  include Lwt_react.S

  let all (ss : 'a t list) : 'a list t = merge (flip List.cons) [] (List.rev ss)

  (** [from' ~placeholder promise] creates a signal that holds the [placeholder]
      until the [promise] resolves. This is similar to {!from} except it does
      only one update. *)
  let from' (placeholder : 'a) (promise : 'a Lwt.t) : 'a t =
    (* Inspect the state: already resolved promises do not need any special
       construction, and this way we avoid weird flickers. *)
    match Lwt.state promise with
    | Return result -> const result
    | Fail _ ->
      Lwt.async (fun () -> Lwt.bind promise (fun _ -> assert false));
      const placeholder
    | Sleep ->
      let result, send_result = create placeholder in
      (* NOTE: Exceptions during {!Lwt.async} are passed to the
         {!Lwt.async_exception_hook}. *)
      Lwt.async
        (fun () ->
          Lwt.bind promise @@ fun value ->
          send_result value;
          stop result;
          lwt_unit
        );
      result

  (** [bind_s' signal placeholder promise] is a signal that begins by holding
      [placeholder]. For all the values held by [signal], [promise] is called
      such that, upon resolution, the output signal gets a new value.

      NOTE: Since [signal] holds a value at the beginning of times, the
      computation starts right away but, because of Lwt, it might still take a
      while before the first value is computed. For this reason, {!bind_s}
      returns a ['b signal Lwt.t]. We prefer to return simply a signal and
      therefore we choose the placeholder approach. *)
  let bind_s' (signal : 'a Lwt_react.signal) (placeholder : 'b) (promise : 'a -> 'b Lwt.t) : 'b Lwt_react.signal =
    switch (from' (const placeholder) (bind_s signal (Lwt.map const % promise)))

  let delayed_setter delay set_immediately =
    let (setter, set_setter) = create lwt_unit in
    fun x ->
      (* try cancelling the current search text setter *)
      Lwt.cancel (value setter);
      (* prepare the new search text setter *)
      let new_setter =
        Js_of_ocaml_lwt.Lwt_js.sleep delay;%lwt
        set_immediately x;
        lwt_unit
      in
      (* register it in the signal *)
      set_setter new_setter;
      (* fire it asynchronously *)
      Lwt.async (fun () -> new_setter)
end

(** {2 TyXML aliases} *)

module C = Js_of_ocaml_tyxml.Tyxml_js.Html
include C
(** Constant HTML nodes. *)

(** Reactive HTML nodes, based on {!React}'s signals. *)
module R = struct
  module R = Js_of_ocaml_tyxml.Tyxml_js.R.Html

  let txt = R.txt

  let a_class elts = R.a_class elts
  let a_style elts = R.a_style elts
  let a_href val_ = R.a_href val_
  let a_value val_ = R.a_value val_

  let div ?a elts = R.div ?a (RList.from_signal elts)
  let span ?a elts = R.span ?a (RList.from_signal elts)
  let tbody ?a elts = R.tbody ?a (RList.from_signal elts)
  let ul ?a elts = R.ul ?a (RList.from_signal elts)
  let ol ?a elts = R.ol ?a (RList.from_signal elts)
  let li ?a elts = R.li ?a (RList.from_signal elts)
  let td ?a elts = R.td ?a (RList.from_signal elts)
  let a ?a elts = R.a ?a (RList.from_signal elts)

  let h1 ?a elts = R.h1 ?a (RList.from_signal elts)
  let h2 ?a elts = R.h2 ?a (RList.from_signal elts)
  let h3 ?a elts = R.h3 ?a (RList.from_signal elts)
  let h4 ?a elts = R.h4 ?a (RList.from_signal elts)
  let h5 ?a elts = R.h5 ?a (RList.from_signal elts)
  let h6 ?a elts = R.h6 ?a (RList.from_signal elts)
end

let span_placeholder ?(min = 4) ?(max = 8) () =
  let col_n = "col-" ^ string_of_int (Random.int_in_range ~min ~max) in
  span ~a: [a_class ["placeholder"; col_n]] []

let div_placeholder ?(min = 4) ?(max = 8) () =
  let height_n_rem = "height: " ^ string_of_int (Random.int_in_range ~min ~max) ^ "rem;" in
  div ~a: [a_class ["placeholder"; "w-100"]; a_style height_n_rem] []

let with_span_placeholder ?min ?max promise =
  R.span @@ S.from' [span_placeholder ?min ?max ()] promise

let with_div_placeholder ?min ?max promise =
  R.div @@ S.from' [div_placeholder ?min ?max ()] promise

module To_dom = Js_of_ocaml_tyxml.Tyxml_js.To_dom
(** Conversion from TyXML nodes to Dom ones. *)

(** Result-Signal monad. *)
module RS = struct
  type 'a t = ('a, string) Result.t S.t

  let pure x =
    S.const (Ok x)

  let bind x f =
    S.bind x @@ function
      | Ok x -> f x
      | Error msg -> S.const @@ Error msg
end
