(** {1 Pair component}

    In itself, this component is not very interesting. It is only meant as a
    building block for more complex components, such as the editor and the
    parameteriser.

    It is as symmetric as possible, but prioritises the first component when one
    must make a choice. This is for instance the case in [focus] and [trigger].
    It does not implement [inner_html] because the choice there is relly not
    obvious. *)

open Nes
open Html

module type S = sig
  type value1
  type value2
  type raw_value1
  type raw_value2

  include Component.S with
  type value = value1 * value2
  and type raw_value = raw_value1 * raw_value2

  val c1 : t -> (value1, raw_value1) Component.t
  val c2 : t -> (value2, raw_value2) Component.t
end

let prepare (type value1)(type raw_value1)(type value2)(type raw_value2)
  ((module C1): (value1, raw_value1) Component.s)
  ((module C2): (value2, raw_value2) Component.s)
  : (module S with
  type value1 = value1
  and type value2 = value2
  and type raw_value1 = raw_value1
  and type raw_value2 = raw_value2)
= (module struct
  let label = "Pair"

  module C1 = C1
  module C2 = C2

  type value1 = C1.value
  type value2 = C2.value
  type raw_value1 = C1.raw_value
  type raw_value2 = C2.raw_value

  type value = C1.value * C2.value
  type raw_value = C1.raw_value * C2.raw_value
  [@@deriving yojson]

  let empty_value =
    (C1.empty_value, C2.empty_value)

  let raw_value_from_initial_text text =
    (C1.raw_value_from_initial_text text, C2.empty_value)

  let serialise (v1, v2) =
    let%lwt v1 = C1.serialise v1 in
    let%lwt v2 = C2.serialise v2 in
    lwt (v1, v2)

  type t = {
    c1: (value1, raw_value1) Component.t;
    c2: (value2, raw_value2) Component.t;
  }

  let c1 p = p.c1
  let c2 p = p.c2

  let signal p =
    RS.bind (Component.signal p.c1) @@ fun v1 ->
    RS.bind (Component.signal p.c2) @@ fun v2 ->
    RS.pure (v1, v2)

  let raw_signal p =
    S.bind (Component.raw_signal p.c1) @@ fun v1 ->
    S.bind (Component.raw_signal p.c2) @@ fun v2 ->
    S.const (v1, v2)

  let focus p = Component.focus p.c1
  let trigger p = Component.trigger p.c1

  let set p (v1, v2) =
    Component.set p.c1 v1;
    Component.set p.c2 v2

  let clear p =
    Component.clear p.c1;
    Component.clear p.c2

  let inner_html _ =
    failwith
      "The Pair component does not implement `inner_html` because there \
              is no obvious way how to do that with a pair."

  let actions _ =
    failwith
      "The Pair component does not implement `actions` because there is \
              no obvious way how to do that with a pair."

  let initialise (initial_value1, initial_value2) =
    let%lwt c1 = Component.initialise (module C1) initial_value1 in
    let%lwt c2 = Component.initialise (module C2) initial_value2 in
    lwt {c1; c2}
end)
