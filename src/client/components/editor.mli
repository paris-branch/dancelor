(** {1 A fully featured editor}*)

open Html

(** {2 Bundle of components}

    An editor features several components, and therefore we provide here a way
    to bundle components together under a list-like structure. *)

type ('value, 'state) bundle

val nil : (unit, unit) bundle

val cons :
  ('value1, 'state1) Component.s ->
  ('value2, 'state2) bundle ->
  ('value1 * 'value2, 'state1 * 'state2) bundle

val (^::):
  ('value1, 'state1) Component.s ->
  ('value2, 'state2) bundle ->
  ('value1 * 'value2, 'state1 * 'state2) bundle
(** [c ^:: cs] is an alias for [cons c cs]. It is right associative. *)

(** {2 High-level interface} *)

type ('result, 'state) mode =
  | QuickEdit of 'state
  | QuickCreate of string * ('result -> unit)
  | CreateWithLocalStorage
  | Edit of 'result
[@@deriving variants]

val make_page :
  key: string ->
  icon: string ->
  assemble: ('value -> 'product) ->
  submit: (('result, 'state) mode -> 'product -> 'result Lwt.t) ->
  unsubmit: ('result -> 'product Lwt.t) ->
  disassemble: ('product -> 'value Lwt.t) ->
  ?preview: ('product -> bool Lwt.t) ->
  format: ('result -> Html_types.div_content_fun Html.elt) ->
  href: ('result -> string) ->
  (* FIXME: URI? *)
  mode: ('result, 'state) mode ->
  ('value, 'state) bundle ->
  Page.t Lwt.t
(** Make a fully-featured editor that takes a whole page.

    [mode] is an argument that decribes what kind of editor we want: is it the
    regular creation editor, connected to local storage, or is it a quick
    creation (eg. called by another editor), in which case it can carry a
    string. It is also passed to the [submit] function.

    Note the different “levels” of values. ['state] and ['value] are tied by the
    underlying component. ['value] is typically of the form [(type, (type,
    (type, ())))]. ['product] is the same but with more structure, depending on
    what the “flat” value represents. ['result] is whatever the API call
    returns. Note the functions allowing to go from low to high: [~assemble]
    allows to go from ['value] to ['product], and [~submit] allows to go from
    ['product] to ['result]. Their counterparts also exist: [~unsubmit] and
    [~disassemble].

    [~disassemble] is allowed to raise {!NonConvertible}. *)

(** {2 Advanced use} *)

val update_local_storage :
  key: string ->
  ('value, 'state) bundle ->
  ('state -> 'state) ->
  unit

exception NonConvertible
(** Exception thrown when trying to edit a model that uses features that the
    editor cannot express. *)

(** {2 Low-level interface}

    This interface is similar to that of components, in the sense that there are
    un-initialised and initialised variants of editors and that one can
    manipulate them. {!make_page} above is the combination of {!prepare},
    {!initialise} and {!page} below *)

type ('result, 'product, 'value, 'state) s
(** An un-initialised editor. *)

val prepare :
  key: string ->
  icon: string ->
  assemble: ('value -> 'product) ->
  submit: (('result, 'state) mode -> 'product -> 'result Lwt.t) ->
  unsubmit: ('result -> 'product Lwt.t) ->
  disassemble: ('product -> 'value Lwt.t) ->
  ?preview: ('product -> bool Lwt.t) ->
  format: ('result -> Html_types.div_content_fun Html.elt) ->
  href: ('result -> string) ->
  ('value, 'state) bundle ->
  ('result, 'product, 'value, 'state) s

val prepare_nosubmit :
  key: string ->
  icon: string ->
  assemble: ('value -> 'result) ->
  disassemble: ('result -> 'value Lwt.t) ->
  ?preview: ('result -> bool Lwt.t) ->
  format: ('result -> Html_types.div_content_fun Html.elt) ->
  href: ('result -> string) ->
  ('value, 'state) bundle ->
  ('result, 'result, 'value, 'state) s
(** Variant of {!prepare} for an editor that does not include submission. In
    this case, the ['product] and the ['result] are conflated. *)

type ('result, 'product, 'value, 'state) t
(** An initialised editor. *)

val initialise :
  ('result, 'product, 'value, 'state) s ->
  ('result, 'state) mode ->
  ('result, 'product, 'value, 'state) t Lwt.t

val page :
  ?after_save: (unit -> unit) ->
  ('result, 'product, 'value, 'state) t ->
  Page.t Lwt.t
(** Render an initialised editor as a full page, ready for use. The additional
    [?after_save] argument can be used to trigger an action from the outside,
    such as closing the dialog. By default, it clears the editor. If overridden,
    one might want to do that manually. *)

(** {3 Editor manipulation} *)

val empty :
  ('result, 'product, 'value, 'state) s ->
  'state

val state_of_yojson :
  ('result, 'product, 'value, 'state) s ->
  Yojson.Safe.t ->
  ('state, string) result

val state_to_yojson :
  ('result, 'product, 'value, 'state) s ->
  'state ->
  Yojson.Safe.t

val result_to_state :
  ('result, 'product, 'value, 'state) s ->
  'result ->
  'state Lwt.t

val state :
  ('result, 'product, 'value, 'state) t ->
  'state S.t

val signal :
  ('result, 'product, 'value, 'state) t ->
  ('product, string) result S.t

val set :
  ('result, 'product, 'value, 'state) t ->
  'result ->
  unit Lwt.t

val clear :
  ('result, 'product, 'value, 'state) t ->
  unit
