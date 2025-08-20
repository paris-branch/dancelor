(** {1 A fully featured editor}*)

open Html

(** {2 Bundle of components}

    An editor features several components, and therefore we provide here a way
    to bundle components together under a list-like structure. *)

type ('value, 'raw_value) bundle

val nil : (unit, unit) bundle

val cons :
  ('value1, 'raw_value1) Component.s ->
  ('value2, 'raw_value2) bundle ->
  ('value1 * 'value2, 'raw_value1 * 'raw_value2) bundle

val (^::):
  ('value1, 'raw_value1) Component.s ->
  ('value2, 'raw_value2) bundle ->
  ('value1 * 'value2, 'raw_value1 * 'raw_value2) bundle
(** [c ^:: cs] is an alias for [cons c cs]. It is right associative. *)

(** {2 High-level interface} *)

type ('result, 'raw_value) mode =
  | QuickEdit of 'raw_value
  | QuickCreate of string * ('result -> unit)
  | CreateWithLocalStorage
  | Edit of 'result
[@@deriving variants]

val make_page :
  key: string ->
  icon: string ->
  preview: ('value -> 'previewed_value option Lwt.t) ->
  submit: (('result, 'raw_value) mode -> 'previewed_value -> 'result Lwt.t) ->
  break_down: ('result -> 'value Lwt.t) ->
  format: ('result -> Html_types.div_content_fun Html.elt) ->
  href: ('result -> string) ->
  (* FIXME: URI? *)
  mode: ('result, 'raw_value) mode ->
  ('value, 'raw_value) bundle ->
  Page.t Lwt.t
(** Make a fully-featured editor that takes a whole page.

    [mode] is an argument that decribes what kind of editor we want: is it the
    regular creation editor, connected to local storage, or is it a quick
    creation (eg. called by another editor), in which case it can carry a
    string. It is also passed to the [submit] function.

    [break_down] is a function able to go in the reverse direction from
    [submit]. In the case of models, for instance, that means breaking down the
    model into values for the components, and is used when editing models. This
    function is allowed to raise {!NonConvertible}. *)

(** {2 Advanced use} *)

val update_local_storage :
  key: string ->
  ('value, 'raw_value) bundle ->
  ('raw_value -> 'raw_value) ->
  unit

exception NonConvertible
(** Exception thrown when trying to edit a model that uses features that the
    editor cannot express. *)

(** {2 Low-level interface}

    This interface is similar to that of components, in the sense that there are
    un-initialised and initialised variants of editors and that one can
    manipulate them. {!make_page} above is the combination of {!prepare},
    {!initialise} and {!page} below *)

type ('result, 'previewed_value, 'value, 'raw_value) s
(** An un-initialised editor. *)

val prepare :
  key: string ->
  icon: string ->
  preview: ('value -> 'previewed_value option Lwt.t) ->
  submit: (('result, 'raw_value) mode -> 'previewed_value -> 'result Lwt.t) ->
  break_down: ('result -> 'value Lwt.t) ->
  format: ('result -> Html_types.div_content_fun Html.elt) ->
  href: ('result -> string) ->
  ('value, 'raw_value) bundle ->
  ('result, 'previewed_value, 'value, 'raw_value) s

type ('result, 'previewed_value, 'value, 'raw_value) t
(** An initialised editor. *)

val initialise :
  ('result, 'previewed_value, 'value, 'raw_value) s ->
  ('result, 'raw_value) mode ->
  ('result, 'previewed_value, 'value, 'raw_value) t Lwt.t

val page :
  ?after_save: (unit -> unit) ->
  ('result, 'previewed_value, 'value, 'raw_value) t ->
  Page.t Lwt.t
(** Render an initialised editor as a full page, ready for use. The additional
    [?after_save] argument can be used to trigger an action from the outside,
    such as closing the dialog. By default, it clears the editor. If overridden,
    one might want to do that manually. *)

(** {3 Editor manipulation} *)

val empty_value :
  ('result, 'previewed_value, 'value, 'raw_value) s ->
  'raw_value

val raw_value_of_yojson :
  ('result, 'previewed_value, 'value, 'raw_value) s ->
  Yojson.Safe.t ->
  ('raw_value, string) result

val raw_value_to_yojson :
  ('result, 'previewed_value, 'value, 'raw_value) s ->
  'raw_value ->
  Yojson.Safe.t

val serialise :
  ('result, 'previewed_value, 'value, 'raw_value) s ->
  'result ->
  'raw_value Lwt.t

val raw_signal :
  ('result, 'previewed_value, 'value, 'raw_value) t ->
  'raw_value S.t

val signal :
  ('result, 'previewed_value, 'value, 'raw_value) t ->
  ('result, string) result S.t
(** NOTE: Using this signal will keep triggering the previsualisation and
    submission on every change. Use only on degenerated editors where those
    functions trivial. FIXME: we should have a type for this. *)

val set_raw_value :
  ('result, 'previewed_value, 'value, 'raw_value) t ->
  'raw_value ->
  unit

val clear : ('result, 'previewed_value, 'value, 'raw_value) t -> unit

val result :
  ('result, 'previewed_value, 'value, 'raw_value) t ->
  'result option Lwt.t
(** Get the current result of the editor, if it is in a state that permits it.
    Note that this trigger previewing and submitting and is therefore usually
    not suitable, except for degenerate editors such as the version parameters
    editor. *)
