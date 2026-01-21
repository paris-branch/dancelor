open Nes
open Html

module Tuple_elt = struct
  type _ t =
    | Zero : 'a -> ('a * 'b) t
    | Succ : 'b t -> ('a * 'b) t

  let zero x = Zero x
  let succ e = Succ e
  let one x = Succ (Zero x)
  let two x = Succ (Succ (Zero x))
  let three x = Succ (Succ (Succ (Zero x)))

  (** It is safe to consume a [unit t] because nothing can construct it.
      Morally, this can be thought of as [-1]. *)
  let consume_negative : unit t -> 'a = fun _ -> assert false

  (** The position of the tuple element in the tuple. *)
  let rec position : type payload. payload t -> int = function
    | Zero _ -> 0
    | Succ b -> 1 + position b
end

module Bundle = struct
  module type Bundle = sig
    type value

    type state [@@deriving yojson]
    (** Typically, this will be a tuple of the form (_, (_, (_, ()))). *)

    val empty : state
    val from_initial_text : string -> state

    val value_to_state : value Tuple_elt.t -> state Lwt.t
    (** Given one value among a tuple, create the state where everything is
        empty, except for that one value where we call the corresponding
        {!Component.S.value_to_state}. *)

    type t

    val initialise : state -> t Lwt.t

    val state : t -> state S.t

    val signal : t -> int -> (value Tuple_elt.t, string) result S.t
    (** Value of the component at the given index, or the error of that component.
        The value is returned as a {!tuple_elt}. *)

    val actions : t -> int -> Html_types.div_content_fun elt list S.t
    (** Actions of the component at the given index. *)

    val inner_html : t -> int -> Html_types.div_content_fun elt
    (** Inner HTML of the component at the given index. *)

    val choices :
      ?offset: int ->
      ?initial_selected: int ->
      t ->
      int option Choices.choice list
    (** One choice per component, starting from the given index. The [?offset]
        argument should only be used by the {!cons} function. *)

    val set : t -> value Tuple_elt.t -> unit Lwt.t
    (** Given one value among a tuple, set the whole bundle to empty everywhere,
        except for that one value. *)

    val clear : t -> unit Lwt.t
    (** Clear all the components of the bundle. *)
  end

  type ('value, 'state) t = (module Bundle with type value = 'value and type state = 'state)

  let cons (type value1)(type value2)(type state1)(type state2)
    ((module A): (value1, state1) Component.s)
    ((module B): (value2, state2) t)
    : (value1 * value2, state1 * state2) t
  = (module struct

    type value = value1 * value2
    type state = A.state * B.state [@@deriving yojson]

    let empty = (A.empty, B.empty)
    let from_initial_text text = (A.from_initial_text text, B.from_initial_text text)

    let value_to_state = function
      | Tuple_elt.Zero v -> let%lwt a_state = A.value_to_state v in lwt (a_state, B.empty)
      | Tuple_elt.Succ elt -> let%lwt b_state = B.value_to_state elt in lwt (A.empty, b_state)

    type t = {a: A.t; b: B.t}

    let state t =
      S.bind (A.state t.a) @@ fun a_state ->
      S.bind (B.state t.b) @@ fun b_state ->
      S.const (a_state, b_state)

    let signal t = function
      | 0 -> S.map (Result.map Tuple_elt.zero) (A.signal t.a)
      | n -> S.map (Result.map Tuple_elt.succ) (B.signal t.b (n - 1))

    let initialise (a_state, b_state) =
      let%lwt a = A.initialise a_state in
      let%lwt b = B.initialise b_state in
      lwt {a; b}

    let actions t = function
      | 0 -> A.actions t.a
      | n -> B.actions t.b (n - 1)

    let inner_html t = function
      | 0 -> A.inner_html t.a
      | n -> B.inner_html t.b (n - 1)

    let choices ?(offset = 0) ?initial_selected t =
      Choices.choice'
        ~value: offset
        ~checked: (initial_selected = Some offset)
        [txt A.label]
        ~onclick: (fun () -> A.trigger t.a) :: B.choices ~offset: (offset + 1) ?initial_selected t.b

    let set t = function
      | Tuple_elt.Zero v -> A.set t.a v;%lwt B.clear t.b;%lwt lwt_unit
      | Tuple_elt.Succ elt -> A.clear t.a;%lwt B.set t.b elt

    let clear t =
      A.clear t.a;%lwt
      B.clear t.b
  end)

  let (^::) = cons

  let nil : (unit, unit) t = (module struct
    type value = unit
    type state = unit [@@deriving yojson]
    let empty = ()
    let from_initial_text _ = ()
    let value_to_state = Tuple_elt.consume_negative
    type t = unit
    let state _ = S.const ()
    let signal _ = invalid_arg "Components.Plus.zero.signal"
    let initialise () = lwt_unit
    let actions _ _ = invalid_arg "Components.Plus.zero.actions"
    let inner_html _ _ = invalid_arg "Components.Plus.zero.inner_html"
    let choices ?offset: _ ?initial_selected: _ () = []
    let set _ _ = lwt_unit
    let clear _ = lwt_unit
  end)
end

let prepare (type value)(type bundled_value)(type state)
  ~label
  ~(cast : bundled_value Tuple_elt.t -> value)
  ~(uncast : value -> bundled_value Tuple_elt.t)
  ?(selected_when_empty : int option)
  (bundle : (bundled_value, state) Bundle.t)
  : (value, int option * state) Component.s
= (module struct
  module Bundle = (val bundle)

  let label = label

  type nonrec value = value
  type state = int option * Bundle.state [@@deriving yojson]

  let value_to_string _value = lwt "<FIXME plus>"

  let value_to_state value =
    let elt = uncast value in
    let selected = Some (Tuple_elt.position elt) in
    let%lwt bundle_state = Bundle.value_to_state elt in
    lwt (selected, bundle_state)

  let empty = (selected_when_empty, Bundle.empty)
  let from_initial_text text = (selected_when_empty, Bundle.from_initial_text text)

  type t = {
    choices: (int, string) Component.t;
    bundle: Bundle.t;
  }

  let selected t = S.map Result.to_option (Component.signal t.choices)

  let state t =
    S.bind (selected t) @@ fun selected ->
    S.bind (Bundle.state t.bundle) @@ fun bundle_state ->
    S.const (selected, bundle_state)

  let signal t =
    S.bind (selected t) @@ function
      | None -> S.const (Error ("You must select a " ^ String.lowercase_ascii label ^ "."))
      | Some selected -> S.map (Result.map cast) (Bundle.signal t.bundle selected)

  let initialise (initial_selected, initial_states) =
    let%lwt bundle = Bundle.initialise initial_states in
    let%lwt choices =
      Choices.make_radios'
        ~label
        ~validate: (Option.to_result ~none: "You must make a choice.")
        (Bundle.choices ?initial_selected bundle)
    in
    lwt {choices; bundle}

  let inner_html p =
    div [
      div ~a: [a_class ["row"]] [
        div ~a: [a_class ["col"]] [Component.inner_html p.choices];
        R.div ~a: [a_class ["col-auto"]] (
          S.bind (Component.signal p.choices) @@ function
            | Error _ -> S.const []
            | Ok n -> Bundle.actions p.bundle n
        );
      ];
      R.div ~a: [a_class ["ps-2"; "mt-1"; "border-start"]] (
        flip S.map (Component.signal p.choices) @@ function
          | Error _ -> []
          | Ok n -> [Bundle.inner_html p.bundle n]
      );
    ]

  let actions _ = S.const []

  let focus t = Component.focus t.choices
  let trigger t = Component.trigger t.choices

  let set t v =
    let v = uncast v in
    Component.set t.choices (Tuple_elt.position v);%lwt
    Bundle.set t.bundle v

  let clear t =
    Component.clear t.choices;%lwt
    Bundle.clear t.bundle
end)
