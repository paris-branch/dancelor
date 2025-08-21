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
  preview: ('value -> 'previewed_value option Lwt.t) ->
  submit: (('result, 'state) mode -> 'previewed_value -> 'result Lwt.t) ->
  break_down: ('result -> 'value Lwt.t) ->
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

    [break_down] is a function able to go in the reverse direction from
    [submit]. In the case of models, for instance, that means breaking down the
    model into values for the components, and is used when editing models. This
    function is allowed to raise {!NonConvertible}. *)

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

type ('result, 'previewed_value, 'value, 'state) s
(** An un-initialised editor. *)

val prepare :
  key: string ->
  icon: string ->
  preview: ('value -> 'previewed_value option Lwt.t) ->
  submit: (('result, 'state) mode -> 'previewed_value -> 'result Lwt.t) ->
  break_down: ('result -> 'value Lwt.t) ->
  format: ('result -> Html_types.div_content_fun Html.elt) ->
  href: ('result -> string) ->
  ('value, 'state) bundle ->
  ('result, 'previewed_value, 'value, 'state) s

type ('result, 'previewed_value, 'value, 'state) t
(** An initialised editor. *)

val initialise :
  ('result, 'previewed_value, 'value, 'state) s ->
  ('result, 'state) mode ->
  ('result, 'previewed_value, 'value, 'state) t Lwt.t

val page :
  ?after_save: (unit -> unit) ->
  ('result, 'previewed_value, 'value, 'state) t ->
  Page.t Lwt.t
(** Render an initialised editor as a full page, ready for use. The additional
    [?after_save] argument can be used to trigger an action from the outside,
    such as closing the dialog. By default, it clears the editor. If overridden,
    one might want to do that manually. *)

(** {3 Editor manipulation} *)

val empty :
  ('result, 'previewed_value, 'value, 'state) s ->
  'state

val state_of_yojson :
  ('result, 'previewed_value, 'value, 'state) s ->
  Yojson.Safe.t ->
  ('state, string) result

val state_to_yojson :
  ('result, 'previewed_value, 'value, 'state) s ->
  'state ->
  Yojson.Safe.t

val serialise :
  ('result, 'previewed_value, 'value, 'state) s ->
  'result ->
  'state Lwt.t

val state :
  ('result, 'previewed_value, 'value, 'state) t ->
  'state S.t

val signal :
  ('result, 'previewed_value, 'value, 'state) t ->
  ('result, string) result S.t
(** NOTE: Using this signal will keep triggering the previsualisation and
    submission on every change. Use only on degenerated editors where those
    functions trivial. FIXME: we should have a type for this. *)

val set_state :
  ('result, 'previewed_value, 'value, 'state) t ->
  'state ->
  unit

val clear : ('result, 'previewed_value, 'value, 'state) t -> unit

val result :
  ('result, 'previewed_value, 'value, 'state) t ->
  'result option Lwt.t
(** Get the current result of the editor, if it is in a state that permits it.
    Note that this trigger previewing and submitting and is therefore usually
    not suitable, except for degenerate editors such as the version parameters
    editor. *)
