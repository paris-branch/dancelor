(** {1 Component} *)

open Html

(** {2 Regular interface} *)

type ('value, 'raw_value) t

val focus : ('value, 'raw_value) t -> unit
val trigger : ('value, 'raw_value) t -> unit
val clear : ('value, 'raw_value) t -> unit
val signal : ('value, 'raw_value) t -> ('value, string) result S.t
val raw_signal : ('value, 'raw_value) t -> 'raw_value S.t
val set : ('value, 'raw_value) t -> 'raw_value -> unit

val inner_html : ('value, 'raw_value) t -> Html_types.div_content_fun elt

val html : ('value, 'raw_value) t -> [> Html_types.div] elt
(** Render the component as HTML. This is not provided by the component itself,
    but is rather a wrapper shared by all components around their
    {!inner_html}. *)

val html_fake : label: string -> Html_types.div_content_fun elt -> [> Html_types.div] elt
(** Mimmics a component in terms of spacing above and below and position of the
    label. *)

(** {2 Module and functor interface}

    This is the interface that composes well. As a creator of components, this
    is what you want to manipulate. As a consumer of a component, you do not
    need to go into this. *)

module type S = sig
  val label : string

  type value
  type raw_value

  val empty_value : raw_value
  val raw_value_from_initial_text : string -> raw_value
  val raw_value_to_yojson : raw_value -> Yojson.Safe.t
  val raw_value_of_yojson : Yojson.Safe.t -> (raw_value, string) result

  type t

  val make : raw_value -> t

  val signal : t -> (value, string) result S.t
  val raw_signal : t -> raw_value S.t
  val focus : t -> unit
  val set : t -> raw_value -> unit

  val trigger : t -> unit
  (** Trigger the component. For simple components, this is akin to {!focus}.
      For components with a button triggering an action, though, {!focus} will
      only focus the button, while {!trigger} will trigger the action. *)

  val clear : t -> unit
  val inner_html : t -> Html_types.div_content_fun elt
end

type ('value, 'raw_value) s = (module S with type value = 'value and type raw_value = 'raw_value)
(** The type of an un-initialised component. This is the type that composes well
    and that one should provide to eg. {!ComponentList}. *)

val initialise : ('value, 'raw_value) s -> 'raw_value -> ('value, 'raw_value) t
(** Initialise a prepared component. *)

val make : (module S with type value = 'value and type raw_value = 'raw_value) -> 'raw_value -> ('value, 'raw_value) t
(** Combination of {!prepare} and {!initialise}. *)

(** {2 Utilities} *)

val case_errored :
  no: 'b ->
  yes: ('e -> 'b) ->
  ('a, 'e) result S.t ->
  'b S.t
