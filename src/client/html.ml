(** {1 HTML} *)

open Nes
open ReactiveData

(** {2 React aliases} *)

(** Reactive signals. *)
module S = struct
  include Lwt_react.S

  let all (ss : 'a t list) : 'a list t = merge (flip List.cons) [] (List.rev ss)

  (** [from_lwt_stream placeholder stream] creates a signal that holds the
      [placeholder] to start, and updates every time the [stream] resolves to a
      new value. *)
  let from_lwt_stream (type a) (placeholder : a) (stream : a Lwt_stream.t) : a t =
    let placeholder =
      (* Inspect the stream: if there is already a first element, then we take
         that as placeholder, and this way we avoid weird flickers. *)
      NesOption.value (Lwt_stream.get_available_1 stream) ~default: placeholder
    in
    let result, send_result = create placeholder in
    Lwt.async (fun () ->
      Lwt_stream.iter send_result stream;%lwt
      (* when the stream is over, stop the signal as well *)
      lwt @@ stop result
    );
    result

  (** [from' ~placeholder promise] creates a signal that holds the [placeholder]
      until the [promise] resolves. This is similar to {!from} except it does
      only one update. *)
  let from' (placeholder : 'a) (promise : 'a Lwt.t) : 'a t =
    from_lwt_stream placeholder (Lwt_stream.return_lwt promise)

  (** Creates a stream of Lwt values from a signal. The signal value at the time
      of creation will be part of the stream. *)
  let to_lwt (signal : 'a t) : 'a Lwt_stream.t =
    let (stream, push, store_ref) = Lwt_stream.create_with_reference () in
    store_ref (map (push % some) signal);
    stream

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

  let flip_map x f = map f x
end

(** {2 TyXML aliases} *)

module C = struct
  include Js_of_ocaml_tyxml.Tyxml_js.Html

  (** Variant of {!txt} that takes a formatter. *)
  let txtf fmt = kspf txt fmt

  (* Override to enforce type discipline *)
  let a_href = a_href % Uri.to_string
  let a_data = a_data % Uri.to_string
  let audio ?src = audio ?src: (Option.map Uri.to_string src)
  let img ~src = img ~src: (Uri.to_string src)
end
include C
(** Constant HTML nodes. *)

(** Reactive HTML nodes, based on {!React}'s signals. *)
module R = struct
  module R = Js_of_ocaml_tyxml.Tyxml_js.R.Html

  let txt = R.txt

  let a_class elts = R.a_class elts
  let a_style elts = R.a_style elts
  let a_value val_ = R.a_value val_

  (* Override to enforce type discipline *)
  let a_href val_ = R.a_href (S.map Uri.to_string val_)
  let a_data val_ = R.a_data (S.map Uri.to_string val_)

  let div ?a elts = R.div ?a (RList.from_signal elts)
  let section ?a elts = R.section ?a (RList.from_signal elts)
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

let with_span_placeholder ?a ?min ?max promise =
  R.span ?a @@ S.from' [span_placeholder ?min ?max ()] promise

let with_div_placeholder ?a ?min ?max promise =
  R.div ?a @@ S.from' [div_placeholder ?min ?max ()] promise

module L = struct
  let div ?a promise = R.div ?a (S.from' [] (Lwt.pause ();%lwt promise))
  let td ?a promise = R.td ?a (S.from' [] (Lwt.pause ();%lwt promise))
end

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
