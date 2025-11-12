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
  type value
  type value1
  type value2
  type state1
  type state2

  include Component.S with type value := value and type state = state1 * state2

  module C1 : Component.S with type value = value1 and type state = state1
  module C2 : Component.S with type value = value2 and type state = state2

  val c1 : t -> C1.t
  val c2 : t -> C2.t
end

let bundle (type value)(type value1)(type state1)(type value2)(type state2)
  ~(wrap : value1 * value2 -> value)
  ~(unwrap : value -> value1 * value2)
  ((module C1): (value1, state1) Component.s)
  ((module C2): (value2, state2) Component.s)
  : (module S with
  type value = value
  and type value1 = value1
  and type value2 = value2
  and type state1 = state1
  and type state2 = state2)
= (module struct
  let label = "Pair"

  module C1 = C1
  module C2 = C2

  type nonrec value = value
  type value1 = C1.value
  type value2 = C2.value
  type state1 = C1.state
  type state2 = C2.state

  type state = C1.state * C2.state
  [@@deriving yojson]

  let empty =
    (C1.empty, C2.empty)

  let from_initial_text text =
    (C1.from_initial_text text, C2.empty)

  let value_to_string _ = lwt "<FIXME pair>"

  let value_to_state v =
    let (v1, v2) = unwrap v in
    let%lwt v1 = C1.value_to_state v1 in
    let%lwt v2 = C2.value_to_state v2 in
    lwt (v1, v2)

  type t = {c1: C1.t; c2: C2.t} [@@deriving fields]

  let signal p =
    RS.bind (C1.signal p.c1) @@ fun v1 ->
    RS.bind (C2.signal p.c2) @@ fun v2 ->
    RS.pure (wrap (v1, v2))

  let state p =
    S.bind (C1.state p.c1) @@ fun v1 ->
    S.bind (C2.state p.c2) @@ fun v2 ->
    S.const (v1, v2)

  let focus p = C1.focus p.c1
  let trigger p = C1.trigger p.c1

  let set p v =
    let (v1, v2) = unwrap v in
    C1.set p.c1 v1;%lwt
    C2.set p.c2 v2

  let clear p =
    C1.clear p.c1;
    C2.clear p.c2

  let inner_html _ =
    failwith
      "The Pair component does not implement `inner_html` because there \
              is no obvious way how to do that with a pair."

  let actions _ =
    failwith
      "The Pair component does not implement `actions` because there is \
              no obvious way how to do that with a pair."

  let initialise (initial_value1, initial_value2) =
    let%lwt c1 = C1.initialise initial_value1 in
    let%lwt c2 = C2.initialise initial_value2 in
    lwt {c1; c2}
end)

let prepare_wrap (type value1)(type state1)(type value2)(type state2)(type value)
  ~label: the_label
  ~(wrap : value1 * value2 -> value)
  ~(unwrap : value -> value1 * value2)
  ((module C1): (value1, state1) Component.s)
  ((module C2): (value2, state2) Component.s)
  : (value, state1 * state2) Component.s
= (module struct
  include (val bundle ~wrap ~unwrap (module C1) (module C2))

  let label = the_label

  let inner_html p =
    div [
      div [C1.inner_html (c1 p)] ~a: [a_class ["mb-1"]];
      div [C2.inner_html (c2 p)];
    ]

  let actions _ = S.const []
end)

let prepare = prepare_wrap ~wrap: id ~unwrap: id
